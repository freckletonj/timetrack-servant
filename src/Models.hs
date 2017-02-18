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
User json
  email     String
  firstName String Maybe
  lastName  String Maybe
  Primary   email
  deriving Show
  
Login json
  userEmail String
  passHash  String
  Foreign   User fkuser userEmail
  Primary   userEmail

TimeEntry json
  userEmail    String
  clockin      UTCTime
  clockout     UTCTime Maybe
  description  String
  Foreign      User fkuser userEmail
  deriving Show

  
|]

instance FromJWT User
instance ToJWT User

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool


