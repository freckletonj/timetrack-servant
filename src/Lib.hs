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


{-
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
-}
