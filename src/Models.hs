{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Control.Monad.Reader
import           Data.Aeson           (FromJSON, ToJSON)
import Data.Time (UTCTime)

import           Database.Persist.Sql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           GHC.Generics         (Generic)

import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()


import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TimeEntry json
  userId       UserId
  clockin      UTCTime
  clockout     UTCTime Maybe
  description  String
  deriving Show

User json
  firstName String Maybe
  lastName  String Maybe
  email     String
  
|]

instance FromJWT User
instance ToJWT User

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

selNow :: MonadIO m => ReaderT SqlBackend m [Single UTCTime]
selNow = rawSql "select now()" []

allTimeEntries :: App [Entity TimeEntry]
allTimeEntries = runDb (selectList [] [])


