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

import           Database.Persist.Sql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           GHC.Generics         (Generic)

import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()



import Data.UUID
import qualified Data.ByteString as B8
import System.Random

import Config
import Models.Util

-- http://stackoverflow.com/questions/35676855/represent-foreign-key-relationship-in-json-using-servant-and-persistent/35677153
-- https://www.reddit.com/r/haskell/comments/4dhpnz/best_way_of_converting_between_persistent_types/

{-
sqlSettings
{ mpsBackend = t
    , mpsGeneric = False
    , mpsPrefixFields = True
    , mpsEntityJSON = Just EntityJSON
        { entityToJSON = 'entityIdToJSON
        , entityFromJSON = 'entityIdFromJSON
        }
    , mpsGenerateLenses = False
    }
-}



share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User json
  Id        UUID sqltype=uuid default=uuid_generate_v4()
  email     String
  firstName String Maybe
  lastName  String Maybe
  
  UniqueUserEmail email
  deriving Show Eq
  
Login json
  Id        UUID sqltype=uuid default=uuid_generate_v4()
  user      UserId
  passHash  String
  deriving Show Eq
  
TimeEntry json
  Id           UUID sqltype=uuid default=uuid_generate_v4()
  clockin      CUTCTime
  clockout     CUTCTime Maybe
  description  String
  deriving Show Eq

TimeEntryRel json
  Id   UUID sqltype=uuid default=uuid_generate_v4()
  time TimeEntryId
  user UserId
  deriving Show Eq
|]


doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

