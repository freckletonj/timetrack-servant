{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Login where

import Data.Aeson
import Data.Aeson.TH
import Data.Text              (Text, pack, unpack)
import Data.Text.Encoding     (decodeASCII)
import Data.Time              (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import Data.Typeable          (Typeable)
import Data.UUID
import Data.UUID.V4 (nextRandom)

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger   (runStdoutLoggingT, logError)
import Control.Monad.Reader
import Control.Monad.Except   (ExceptT, MonadError)

import GHC.Generics           (Generic)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import Database.Persist.Postgresql
import Database.Persist.Sql (rawSql, SqlPersistT, unSingle, Single, Entity)
import Database.Persist.TH

import Crypto.JOSE.Error (Error)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Data.Bifunctor
import Control.Monad.Trans.Either

import Models
import Config
import Lib

import Debug.Trace (trace)

{-

# Custom Primary Keys
- http://stackoverflow.com/questions/41215452/yesod-querying-persist-database-with-a-custom-primary-key
need custom type XKey = XKey { unXKey :: ... }

# Allowing DB to provide unspecified values (via `default`)
- http://stackoverflow.com/questions/35231697/how-to-let-default-values-come-from-the-database
  - DB Triggers
    - http://stackoverflow.com/questions/1035980/update-timestamp-when-row-is-updated-in-postgresql
  - don't, specify all values in haskell
- https://www.reddit.com/r/haskell/comments/4srvuw/struggling_to_implement_createdat_updatedat/
  - DB Triggers
  - yesod lazy fields
    - https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax#laziness
- https://groups.google.com/forum/#!topic/yesodweb/sLZ53JzIjVQ
  - specify in haskell, don't let the db handle defaults
  - custom fns, eg insertWithUUID :: PersistValue? -> m ()

*** I'm pursuing the "do it in haskell" method

# UUIDs
## Sequential Namespace
- V1 - based on MAC address + system clock - not thread safe, fast - libs like https://github.com/danlentz/clj-uuid can make em thread safe through use of "subcounters" for when the clock isn't sufficiently granular to distinguish two times


## Cryptographic Ids
- V4 - slow, and pseudo-non-deterministic (because of using a pseudo-random seed)

## Namespaced Ids
- V3 - MD5  - faster
- V5 - SHA1 - more secure

seeded with some text, and then deterministically generated


-}


-- TODO: belongs in config
hashIterations = 12         -- 15 =~ 6 sec
tokenDuration = 60*60*24*30 -- one month, TODO: make revokable, expiration works, but can't be revoked that way

--------------------------------------------------
-- Persist Utils

withUUID :: (UUID -> User) -> IO User
withUUID = (nextRandom >>=) . (return .)

  
--------------------------------------------------
-- Login API

data UnsafeLogin = UnsafeLogin { email :: String
                               , clearPass :: String}
                 deriving (Generic, FromJSON)

data Token = Token {
  userId :: UserId
  } deriving (Generic, ToJSON, FromJSON)
instance FromJWT Token where
instance ToJWT   Token where

type LoginAPI = "token" :> ReqBody '[JSON] UnsafeLogin
                        :> Post '[JSON] (String)
                :<|> "signup" :> ReqBody '[JSON] UnsafeLogin
                              :> Post '[JSON] (Key Login)
  
loginServerT :: JWTSettings -> ServerT LoginAPI App
loginServerT jwts = login :<|> new
  where
    login :: UnsafeLogin -> App String
    login (UnsafeLogin email p) =
          fetchLogin
      >=> (validateHash p)
      >=> (\(Login uid _) ->
             (getToken jwts (Token uid)))
      $ email
     
    new :: UnsafeLogin -> App (Key Login)
    new (UnsafeLogin e p) = do
      k <- runDb . insert $ (User e Nothing Nothing)
      hashed <- liftIO $ hashPassword hashIterations (B.pack p)
      k' <- runDb . insert $ Login k $ B.unpack hashed
      return k'

loginServerToHandler :: Config -> App :~> ExceptT ServantErr IO
loginServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

loginServer :: JWTSettings -> Config -> Server LoginAPI
loginServer jwts cfg = enter (loginServerToHandler cfg) (loginServerT jwts)

--------------------------------------------------

-- TODO: this is 2 calls to the db, silly
fetchLogin :: String -> App Login
fetchLogin e =  do
  (Entity uid u) <- maybe (throwError err401) return
    =<< (runDb (selectFirst [UserEmail ==. e] []))
  (Entity _ l) <- maybe (throwError err401) return
    =<< (runDb (selectFirst [LoginUser ==. uid] []))
  return l

validateHash :: String -> Login -> App Login
validateHash p login@(Login _ hp) = trace hp $
                                    if validatePassword (B.pack p) (B.pack hp)
                                    then return login
                                    else throwError err401

getToken :: (ToJWT a) => JWTSettings -> a -> App String
getToken jwts obj = either (\_ -> throwError err401)
                    (return . B.unpack . BL.toStrict)
                    =<< liftIO (returnJwt jwts obj)

returnJwt :: (ToJWT a) =>  JWTSettings -> a -> IO (Either Error BL.ByteString)
returnJwt jwts obj = do
  now <- getCurrentTime
  let expiry = addUTCTime tokenDuration now
  return =<< makeJWT obj jwts (Just expiry)
