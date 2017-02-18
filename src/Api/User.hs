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
import Data.Time              (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import Data.Typeable          (Typeable)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger   (runStdoutLoggingT)
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
                                   , Entity
                                   , SqlBackend
                                   , selectList)

import Database.Persist.Sql (rawSql, SqlPersistT, unSingle, Single)
import Database.Persist.TH

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString.Char8 as B

import Models
import Config
import Lib

data UnsafeLogin = UnsafeLogin { email :: String
                               , clearPass :: String} deriving (Generic)
instance FromJSON UnsafeLogin

type LoginAPI = "login" :> ReqBody '[JSON] UnsafeLogin
                        :> Post '[JSON] (String)
              :<|> "new" :> ReqBody '[JSON] UnsafeLogin
                         :> Post '[JSON] (Key Login)

hashIterations = 12 -- 15 =~ 6 sec

loginServerT :: ServerT LoginAPI App
loginServerT = login :<|> new
  where
    login :: UnsafeLogin -> App String
    login (UnsafeLogin e p) = do
        case (keyFromValues [PersistText $ pack e]) of
          Left e -> error $ unpack e -- shouldn't happen
          Right k -> do
            mu <- (runDb (get k)) :: (App (Maybe Login))
            case mu of
              Nothing -> error "wrong username or pass" -- couldn't find user
              Just (Login _ hp) -> do
                if validatePassword (B.pack p) (B.pack hp)
                then return "success"
                else return "fail"

    new :: UnsafeLogin -> App (Key Login)
    new (UnsafeLogin e p) = do
      k <- runDb . insert $ (User e Nothing Nothing)
      hashed <- liftIO $ hashPassword hashIterations (B.pack p)
      k' <- runDb . insert $ (Login e $ B.unpack hashed)
      return k'

loginServerToHandler :: Config -> App :~> ExceptT ServantErr IO
loginServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

loginServer :: Config -> Server LoginAPI
loginServer cfg = enter (loginServerToHandler cfg) loginServerT



----------

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



