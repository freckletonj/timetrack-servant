{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Api.TimeEntry where

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
                                   , Entity
                                   , SqlBackend
                                   , selectList)

import Database.Persist.Sql (rawSql, SqlPersistT, unSingle, Single)
import Database.Persist.TH

import Models
import Config
import Lib


--------------------------------------------------
-- Types

-- types imported from Models

times :: [TimeEntry]
times = [ TimeEntry (posixSecondsToUTCTime 0) (Just $ posixSecondsToUTCTime 1) "first"
        , TimeEntry (posixSecondsToUTCTime 2) Nothing "second"]

--------------------------------------------------
-- Misc API

type MiscAPI = "nowIO" :> Get '[JSON] UTCTime
          :<|> "error" :> Get '[JSON] String
          :<|> "printIO" :> Get '[JSON] NoContent

miscServer :: Server MiscAPI
miscServer = now
             :<|> error
             :<|> printer
  where
        now :: Handler UTCTime
        now = liftIO getCurrentTime >>= return
        error :: Handler String
        error = throwError err404 { errBody = "a dinosaur ate the server" }
        printer :: Handler NoContent
        printer = liftIO $ putStrLn "printed!" >> return NoContent

--------------------------------------------------
-- ReaderMonad API

type ReaderAPI = Get '[JSON] UTCTime
readerServerT :: ServerT ReaderAPI App
readerServerT = f
  where
    f :: App UTCTime
    f = runDb selNow >>= return . unSingle . head

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerToHandler :: Config -> App :~> ExceptT ServantErr IO
readerToHandler cfg = Nat (flip runReaderT cfg . runApp)

readerServer :: Config -> Server ReaderAPI
readerServer  cfg = enter (readerToHandler cfg) readerServerT



--------------------------------------------------
-- Time API

-- {"clockin": "2013-10-17T09:42:49.007Z",
-- "description": "first success"}

type TimesAPI = Get '[JSON] [Entity TimeEntry] -- list all
                :<|> ReqBody '[JSON] TimeEntry :> Post '[JSON] (Key TimeEntry) -- add a new one
                :<|> Capture "timeid" (Key TimeEntry) :> Get '[JSON] (Maybe TimeEntry) -- get one
                :<|> Capture "timeid" (Key TimeEntry) :> ReqBody '[JSON] TimeEntry :> PutNoContent '[JSON] NoContent -- replace one
                :<|> Capture "timeid" (Key TimeEntry) :> DeleteNoContent '[JSON] NoContent

timesServerT :: ServerT TimesAPI App
timesServerT = listTimes
               :<|> putTime
               :<|> getTime
               :<|> updateTime
               :<|> deleteTime
  where listTimes :: App [Entity TimeEntry]
        listTimes = runDb (selectList [] []) >>= return
        putTime :: TimeEntry -> App (Key TimeEntry)
        putTime te = runDb (insert te) >>= return
        getTime :: (Key TimeEntry) -> App (Maybe TimeEntry)
        getTime i = runDb (get i) >>= return
        updateTime :: (Key TimeEntry) -> TimeEntry -> App NoContent
        updateTime i te = runDb (replace i te) >> return NoContent
        deleteTime :: (Key TimeEntry) -> App NoContent
        deleteTime i = runDb (delete i) >> return NoContent

timesServerToHandler :: Config -> App :~> ExceptT ServantErr IO
timesServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

timesServer :: Config -> Server TimesAPI
timesServer cfg = enter (timesServerToHandler cfg) timesServerT

--------------------------------------------------
-- Auth API

type Protected
    = "name"  :> Get '[JSON] String
 :<|> "email" :> Get '[JSON] String

type Unprotected =
 "login"
     :> ReqBody '[JSON] Login
     :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] String)
  :<|> Raw

protected :: AuthResult User -> Server Protected
protected (Authenticated user) = return (name user) :<|> return (email user)
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = checkCreds cs jwts
                 :<|> serveDirectory "example/static" 


-- for static assets
files :: Application
files = serveDirectory "assets"


