{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

module Models.Util where

import System.Random
import GHC.Generics (Generic)
import Database.Persist.Sql
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, withText, object, Value(String))
import Data.UUID
import Data.Text
import System.Random
import qualified Data.ByteString as B8
import Web.PathPieces (PathPiece(..))

--------------------------------------------------
-- UUID JSON

instance ToJSON UUID where
  toJSON = String . toText
instance FromJSON UUID where
  parseJSON = withText "UUID" $ (\x -> case fromText x of
                                    Just y -> return y
                                    Nothing -> fail "not a uuid")

--------------------------------------------------

-- reference : http://bitemyapp.com/posts/2016-06-15-uuids-with-persistent-yesod.html

instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . toASCIIBytes $ u
  fromPersistValue (PersistDbSpecific t) =
    case fromASCIIBytes t of
      Just x -> Right  x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"
  
instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = toText
  fromPathPiece = fromText


--------------------------------------------------

-- INSTEAD of deriving all of this for UserUUID
-- I'm going instead for just proper UUID instances
-- above

-- newtype UserUUID = UserUUID { userUuid :: UUID }
--                  deriving (Show, Read, Eq, Ord, Random, Generic, ToJSON, FromJSON)

-- instance PersistField UserUUID where
--   toPersistValue u = PersistDbSpecific . toASCIIBytes . userUuid $ u
--   fromPersistValue (PersistDbSpecific t) =
--     case fromASCIIBytes t of
--       Just x -> Right . UserUUID $ x
--       Nothing -> Left "Invalid UUID"
--   fromPersistValue _ = Left "Not PersistDBSpecific"
  
-- instance PersistFieldSql UserUUID where
--   sqlType _ = SqlOther "uuid"

