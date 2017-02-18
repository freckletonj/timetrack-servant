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

module Config where



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


data Env = Development
         | Testing
         | Production

data Config = Config { getPool :: ConnectionPool
                     , getEnv :: Env }

connStr :: ConnectionString
connStr = "host=localhost dbname=timetrack-dev user=timetrack-dev password=pass port=5432"

makePool :: Env -> IO ConnectionPool
makePool Development = runStdoutLoggingT $ createPostgresqlPool connStr 8
makePool Testing     = undefined
makePool Production  = undefined



newtype App a = App { runApp :: ReaderT Config (ExceptT ServantErr IO) a
                    } deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)
