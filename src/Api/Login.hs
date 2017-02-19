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

type LoginAPI = "token" :> ReqBody '[JSON] UnsafeLogin
                        :> Post '[JSON] (String)
                :<|> "signup" :> ReqBody '[JSON] UnsafeLogin
                              :> Post '[JSON] (Key Login)
  
loginServerT :: JWTSettings -> ServerT LoginAPI App
loginServerT jwts = login :<|> new
  where
    login :: UnsafeLogin -> App String
    login (UnsafeLogin email p) =
      createKey
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

loginServerToHandler :: Config -> App :~> ExceptT ServantErr IO
loginServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

loginServer :: JWTSettings -> Config -> Server LoginAPI
loginServer jwts cfg = enter (loginServerToHandler cfg) (loginServerT jwts)

--------------------------------------------------

returnJwt :: (ToJWT a) =>  JWTSettings -> a -> IO (Either Error BL.ByteString)
returnJwt jwts obj = do
  now <- getCurrentTime
  let expiry = addUTCTime tokenDuration now
  return =<< makeJWT obj jwts (Just expiry)

createKey :: (PersistEntity record) => String -> App (Key record)
createKey email = either (const $ throwError err401)
                  return $ keyFromValues [PersistText $ pack email]

fetchPass :: (Key Login) -> App String
fetchPass k = do
  (Login _ hp) <- maybe (throwError err401) return =<< (runDb (get k))
  return hp

validateHash :: String -> String -> App ()
validateHash p hp = if validatePassword (B.pack p) (B.pack hp)
                    then return ()
                    else throwError err401

getToken :: (ToJWT a) => JWTSettings -> a -> App String
getToken jwts obj = either (\_ -> throwError err401)
                    (return . B.unpack . BL.toStrict)
                    =<< liftIO (returnJwt jwts obj)

