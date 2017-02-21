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
-- (runSqlPool
--                                    , (==.)
--                                    , get
--                                    , Entity
--                                    , toSqlKey
--                                    , keyFromValues
--                                    , insert
--                                    , delete
--                                    , update
--                                    , replace
--                                    , deleteWhere
--                                    , ConnectionPool
--                                    , ConnectionString
--                                    , createPostgresqlPool
--                                    , withPostgresqlPool
--                                    , liftSqlPersistMPool
--                                    , PersistValue (PersistText)
--                                    , PersistEntity
--                                    , Entity
--                                    , SqlBackend
--                                    , selectList
--                                    , selectFirst)

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

-- TODO: belongs in config
hashIterations = 12         -- 15 =~ 6 sec
tokenDuration = 60*60*24*30 -- one month, TODO: make revokable, expiration works, but can't be revoked that way

--------------------------------------------------
-- Login API

data UnsafeLogin = UnsafeLogin { email :: String
                               , clearPass :: String} deriving (Generic, FromJSON)
data Token = Token { userId :: UserId } deriving (Generic, ToJSON, FromJSON)

instance FromJWT Token where
instance ToJWT   Token where

  
--instance FromJSON UnsafeLogin 

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
             (getToken jwts (Token uid))) -- TODO: return a better object
      $ email -- this ends up as `createKey`'s param
     
    new :: UnsafeLogin -> App (Key Login)
    new (UnsafeLogin e p) = do
      k <- runDb . insert $ (User e Nothing Nothing)
      hashed <- liftIO $ hashPassword hashIterations (B.pack p)
      k' <- runDb . insert $ (Login k $ B.unpack hashed)
      return k'

loginServerToHandler :: Config -> App :~> ExceptT ServantErr IO
loginServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

loginServer :: JWTSettings -> Config -> Server LoginAPI
loginServer jwts cfg = enter (loginServerToHandler cfg) (loginServerT jwts)

--------------------------------------------------


-- createKey :: (PersistEntity record) => String -> App (Key record)
-- createKey email = trace email $ either (const $ throwError err401)
--                   return $ keyFromValues [PersistText $ pack email]

fetchLogin :: String -> App Login
fetchLogin e =  do
  (Entity uid u) <- maybe (throwError err401) return
           =<< (runDb (selectFirst [UserEmail ==. e] []))
  (Entity _ l) <- maybe (throwError err401) return =<< (runDb (selectFirst [LoginUser ==. uid] []))
  return l
  -- case muser of
  --   Nothing             -> throwError err401
  --   Just (Entity uid u) -> undefined





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
