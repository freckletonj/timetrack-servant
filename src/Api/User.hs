{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.User where

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

import Database.Persist.Postgresql (runSqlPool
                                   , get
                                   , toSqlKey
                                   , keyFromValues
                                   , insert
                                   , delete
                                   , update
                                   , replace
                                   , deleteWhere
                                   , ConnectionPool
                                   , ConnectionString
                                   , createPostgresqlPool
                                   , withPostgresqlPool
                                   , liftSqlPersistMPool
                                   , PersistValue (PersistText)
                                   , PersistEntity
                                   , Entity
                                   , SqlBackend
                                   , selectList)

import Database.Persist.Sql (rawSql, SqlPersistT, unSingle, Single)
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

-- TODO: belongs in config
hashIterations = 12         -- 15 =~ 6 sec
tokenDuration = 60*60*24*30 -- one month, TODO: make revokable, expiration works, but can't be revoked that way

--------------------------------------------------
-- Login API

data UnsafeLogin = UnsafeLogin { email :: String
                               , clearPass :: String} deriving (Generic)
instance FromJSON UnsafeLogin 

type LoginAPI = "login" :> ReqBody '[JSON] UnsafeLogin
                        :> Post '[JSON] (String)
              :<|> "new" :> ReqBody '[JSON] UnsafeLogin
                         :> Post '[JSON] (Key Login)

returnJwt :: (ToJWT a) =>  JWTSettings -> a -> IO (Either Error BL.ByteString)
returnJwt jwts obj = do

  now <- getCurrentTime
  let expiry = addUTCTime tokenDuration now
  etoken <- makeJWT obj
            jwts
            (Just expiry)
  return $ etoken

createKey :: (PersistEntity record) => String -> App (Key record)
createKey email = do
  k  <- return (keyFromValues [PersistText $ pack email])
  k' <- either (const $ throwError err401) return k
  return k'

fetchPass :: (Key Login) -> App String
fetchPass k = do
  mu           <- (runDb (get k))
  (Login _ hp) <- maybe (throwError err401) return mu
  return hp

validateHash :: String -> String -> App ()
validateHash p hp = if validatePassword (B.pack p) (B.pack hp) then return () else throwError err401

getToken :: (ToJWT a) => JWTSettings -> a -> App String
getToken jwts obj = do
  etoken <- liftIO (returnJwt jwts obj)
  either (\_ -> throwError err401) (return . B.unpack . BL.toStrict) etoken

  
loginServerT :: JWTSettings -> ServerT LoginAPI App
loginServerT jwts = login :<|> new
  where
    login :: UnsafeLogin -> App String
    login (UnsafeLogin email p) = createKey
                                  >=> fetchPass
                                  >=> (validateHash p)
                                  >=> const (getToken jwts (User email Nothing Nothing)) -- TODO: return a better object
                                  $ email -- this ends up as `createKey`'s param
     
    new :: UnsafeLogin -> App (Key Login)
    new (UnsafeLogin e p) = do
      k <- runDb . insert $ (User e Nothing Nothing)
      hashed <- liftIO $ hashPassword hashIterations (B.pack p)
      k' <- runDb . insert $ (Login e $ B.unpack hashed)
      return k'

maybeToEither :: err -> Maybe a -> Either err a
maybeToEither = flip maybe Right . Left

loginServerToHandler :: Config -> App :~> ExceptT ServantErr IO
loginServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

loginServer :: JWTSettings -> Config -> Server LoginAPI
loginServer jwts cfg = enter (loginServerToHandler cfg) (loginServerT jwts)


--------------------------------------------------
-- User API

type UserAPI = Get '[JSON] [Entity User]                 -- list all - todo: auth+role
                :<|> ReqBody '[JSON] User
                     :> Post '[JSON] (Key User)          -- add a new one
                :<|> Capture "userid" (Key User) :> 
                (
                  Get '[JSON] (Maybe User)               -- get one - todo: auth
                  :<|> ReqBody '[JSON] User
                       :> PutNoContent '[JSON] NoContent -- replace one - todo: auth
                  :<|> DeleteNoContent '[JSON] NoContent -- delete one - todo: auth
                )

userServerT :: ServerT UserAPI App
userServerT = listUsers
               :<|> putUser
               :<|> (\i ->
                       getUser i
                       :<|> updateUser i
                       :<|> deleteUser i
                    )
  where listUsers :: App [Entity User]
        listUsers = runDb (selectList [] []) >>= return
        putUser :: User -> App (Key User)
        putUser te = runDb (insert te) >>= return
        getUser :: (Key User) -> App (Maybe User)
        getUser i = runDb (get i) >>= return
        updateUser :: (Key User) -> User -> App NoContent
        updateUser i te = runDb (replace i te) >> return NoContent
        deleteUser :: (Key User) -> App NoContent
        deleteUser i = runDb (delete i) >> return NoContent

userServerToHandler :: Config -> App :~> ExceptT ServantErr IO
userServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

userServer :: Config -> Server UserAPI
userServer cfg = enter (userServerToHandler cfg) userServerT
