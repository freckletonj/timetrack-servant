{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- NOTE : for Reader Monad see : https://github.com/haskell-servant/servant/blob/29f94a64408d6a7815dbe43fd06e8b372b3230e9/doc/tutorial/Server.lhs#L1104
--

module Lib where
    

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

--------------------------------------------------
-- Types

-- types imported from Models

times :: [TimeEntry]
times = [ TimeEntry (posixSecondsToUTCTime 0) (Just $ posixSecondsToUTCTime 1) "first"
        , TimeEntry (posixSecondsToUTCTime 2) Nothing "second"]

--------------------------------------------------
-- Misc API

type MiscAPI = "now" :> Get '[JSON] UTCTime
          :<|> "error" :> Get '[JSON] String
          :<|> "print" :> Get '[JSON] NoContent

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
-- Reader API

type ReaderAPI = Get '[JSON] UTCTime
readerServerT :: ServerT ReaderAPI App -- (ReaderT (IO a) Config)
readerServerT = f
  where
    f :: App UTCTime
    f = runDb selNow >>= return . unSingle . head

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

-- readerToHandler' :: Config -> forall a. Reader Config a -> Handler a
-- readerToHandler' cfg r = return (runReader r cfg)
readerToHandler :: Config -> App :~> ExceptT ServantErr IO
readerToHandler cfg = Nat (flip runReaderT cfg . runApp)

readerServer :: Config -> Server ReaderAPI
readerServer  cfg = enter (readerToHandler cfg) readerServerT



--------------------------------------------------
-- Time API

type TimesAPI = Get '[JSON] [TimeEntry]
                -- :<|> Capture "timeid" Int :> Get '[JSON] TimeEntry

timesServer :: Server TimesAPI
timesServer = listTimes
              -- :<|> getTime
  where listTimes :: Handler [TimeEntry]
        listTimes = return times

        -- getTime :: Int -> Handler TimeEntry
        -- getTime i = return $ head $ filter (\t -> timeid t == i) times

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

--------------------------------------------------
-- servant-auth

data User = User { name :: String, email :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Login = Login { username :: String, password :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

checkCreds :: CookieSettings -> JWTSettings -> Login
  -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
checkCreds cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
   let usr = User "Ali Baba" "ali@email.com"
   mcookie <- liftIO $ makeCookie cookieSettings jwtSettings usr
   case mcookie of
     Nothing     -> throwError err401
     Just cookie -> return $ addHeader cookie $ show cookie
checkCreds _ _ _ = throwError err401

--------------------------------------------------
-- RUN
