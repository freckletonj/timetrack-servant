{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
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

--import           Config


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TimeEntry json
  clockin      UTCTime
  clockout     UTCTime Maybe
  description  String
  deriving Show
|]

doMigrations :: SqlPersistM ()
doMigrations = runMigration migrateAll

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- User json
--     name String
--     email String
--     deriving Show
-- |]

-- doMigrations :: SqlPersistM ()
-- doMigrations = runMigration migrateAll

-- runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
-- runDb query = do
--     pool <- asks getPool
--     liftIO $ runSqlPool query pool
