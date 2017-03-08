{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

module Models.Util where

import System.Random
import GHC.Generics (Generic)
import Database.Persist.Sql hiding (UTCTime)
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, withText, object, Value(String))
import Data.UUID
import Data.Text
-- import Data.Time (UTCTime)
import Data.Time (UTCTime(..), picosecondsToDiffTime, diffTimeToPicoseconds) -- constructor

import System.Random
import qualified Data.ByteString as B8
import Web.PathPieces (PathPiece(..))

import Debug.Trace (trace)

--------------------------------------------------
-- UUID JSON

instance ToJSON UUID where
  toJSON = String . toText
instance FromJSON UUID where
  parseJSON = withText "UUID" $ (\x -> case fromText x of
                                    Just y -> return y
                                    Nothing -> fail "not a uuid")

--------------------------------------------------
-- UUID Persist
--   reference :
--     - http://bitemyapp.com/posts/2016-06-15-uuids-with-persistent-yesod.html
--     - http://michaelxavier.net/posts/2015-04-14-Adding-a-UUID-Column-to-a-Persistent-Table.html

instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . toASCIIBytes $ u
  fromPersistValue (PersistDbSpecific t) =
    case fromASCIIBytes t of
      Just x -> Right  x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"
  
instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

-- Yesod-related cruft, but needed for UUIDs to work
--   just converts to/from text for yesod's url paths
instance PathPiece UUID where
  toPathPiece = toText
  fromPathPiece = fromText

--------------------------------------------------
-- Custom UTCTime, since the stock one does weirdness with milliseconds being there, or not

newtype CUTCTime = CUTCTime UTCTime
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance PersistField CUTCTime where
  toPersistValue (CUTCTime (UTCTime d t)) = toPersistValue (UTCTime d (removeMs t))
    where
      removeMs =  fromInteger . floor
  fromPersistValue (PersistUTCTime x) = Right $ CUTCTime x
  
instance PersistFieldSql CUTCTime where
  sqlType _ = SqlDayTime
