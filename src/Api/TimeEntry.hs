{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.TimeEntry where

import Data.Aeson
import Data.Aeson.TH
import Data.Text              (Text, pack, unpack)
import Data.Time              (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import Data.Typeable          (Typeable)
import Data.UUID

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger   (runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.Except   (ExceptT, MonadError)

import Data.Aeson
import Data.Aeson.Types       (Parser, parseMaybe)
import Data.Aeson.Lens
import Control.Lens hiding    ((.=), set, (^.))
import Control.Lens.TH
import GHC.Generics           (Generic)
import GHC.Int                (Int64)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
import Database.Esqueleto

import Models
import Config
import Lib
import Api.Login

--------------------------------------------------
-- Annoying Data Munging

-- TimeEntrys that go over the Wire
--   they're just missing the `user` foreign key
--   and of course the TimeEntryID which Persistent
--   normally handles anyways through `Entity`s

-- data TimeEntryW = TimeEntryW { clockin :: UTCTime
--                              , clockout :: Maybe UTCTime
--                              , description :: String}
--                   deriving (Show, Generic, ToJSON, FromJSON)

-- timeEntryW :: TimeEntry -> TimeEntryW
-- timeEntryW TimeEntry{..} = TimeEntryW {
--   clockin       = timeEntryClockin
--   , clockout    = timeEntryClockout
--   , description = timeEntryDescription
--   }

-- timeEntry :: (Key User) -> TimeEntryW -> TimeEntry
-- timeEntry fk TimeEntryW{..} = TimeEntry {
--   timeEntryUser          = fk
--   , timeEntryClockin     = clockin
--   , timeEntryClockout    = clockout
--   , timeEntryDescription = description
--   }

{-
Design Pattern ------------------------------
-- https://github.com/jhedev/todobackend-haskell/blob/master/todobackend-common/src/TodoBackend/Model.hs

-- Servant
... ReqBody ... ModelXAction :> ... ModelXResponse

-- DB, persistent dresses it with extras, eg a primary key
data ModelX = ModelX { modelXFieldA :: A, modelXFieldB :: B } -- probably defined in Persistent.TH form

-- HTTP
-- response adds id, and some examples add a url path to this resource
data ModelXResponse = ModelXResponse { mxrid :: ModelXId, mxrFieldA :: A, mxrFieldB :: B } deriving (Show)
$(deriveToJSON defaultOptions { fieldLabelModifier = drop (length "mxr") } -- some fn for character casing?
  ''ModelXResponse)

-- Action, great for PATCHes
-- maybe everything!
data ModelXAction = ModelXAction { actFieldA :: Maybe A, actFieldB :: Maybe B } deriving (Show, Generic, FromJSON) -- prob no ToJSON?

-- defaults -- probably not a great idea
actionToDefaultModelX :: ModelXAction -> ModelX

-- PATCHable updates
actionToUpdates :: ModelXAction -> [Update ModelX]
... [ModelXFieldA =. ...] ++ [ModelXFieldB =. ...]

-}

{-
Useful Abstraction to build at some point
https://www.reddit.com/r/haskell/comments/3eylp6/testing_servant_persistent_web_app/

-- CRUD endpoints for entities of type 'a'
-- indexed by values of type 'i'
type CRUD i a = ReqBody '[JSON] a :> POST '[JSON] () -- create
           :<|> Capture "id" i :> Get '[JSON] a -- read
           :<|> Capture "id" i :> ReqBody '[JSON] a :> Put '[JSON] () -- update
           :<|> Capture "id" i :> Delete '[JSON] () -- delete
           -- the last three could be written:
           -- Capture "id" i :> (Get ... :<|> ReqBody ... :> Put ... :<|> Delete ...)

type MyAPI = "users" :> CRUD UserId User
        :<|> "products" :> CRUD ProductId Product

-}


--------------------------------------------------
-- CRUD
-- newtype MKey a = MKey { getMKey :: UUID } deriving (Generic, FromJSON, ToJSON, Show)

-- _MKey :: ToBackendKey SqlBackend a => Iso' (MKey a) (Key a)
-- _MKey = iso (toSqlKey . getMKey) (MKey . fromSqlKey)

-- cruft dependency from Yesod's Persistent
-- instance FromHttpApiData (Key a) where
-- instance ToHttpApiData (Key a) where
  
type CRUD db act = ReqBody '[JSON] db :> Post '[JSON] (Key db)
                   :<|> Capture "id" (Key db) :> (
                             Get '[JSON] (Entity db)
                             :<|> ReqBody '[JSON] act :> Put '[JSON] NoContent
                             :<|> Delete '[JSON] NoContent)
  
--------------------------------------------------
-- TimeEntry Types

{- Model.hs
TimeEntry json
  clockin      UTCTime
  clockout     UTCTime Maybe
  description  String
  deriving Show Eq

TimeEntryRel json
  time TimeEntry
  user UserId
  deriving Show Eq
-}

----------

data TimeEntryAction = TimeEntryAction
  {
    actClockin :: Maybe UTCTime
  , actClockout :: Maybe UTCTime
  , actDescription :: Maybe String
  } deriving (Show)

instance FromJSON TimeEntryAction where
  parseJSON (Object o) = TimeEntryAction
    <$> o .:? "clockin"
    <*> o .:? "clockout"
    <*> o .:? "description"

--actionToUpdates :: TimeEntryAction -> [P.Update TimeEntry]
actiontoupdates TimeEntryAction{..} = updateClockin
                                      ++ updateClockout
                                      ++ updateDescription
  where
    updateClockin     = maybe [] (\x -> [TimeEntryClockin     =. val x])       actClockin
    updateClockout    = maybe [] (\x -> [TimeEntryClockout    =. val (Just x)]) actClockout
    updateDescription = maybe [] (\x -> [TimeEntryDescription =. val x])   actDescription

    
--------------------------------------------------
-- Api

type TimesAPI =
  -- List All
  Get '[JSON] [Entity TimeEntry]
  :<|> CRUD TimeEntry TimeEntryAction
  
timesServerT :: AuthResult Token -> ServerT TimesAPI App
timesServerT (Authenticated tok)  =
  listTimes
  :<|> (createTime
        :<|> (\ti ->
                getTime ti
               :<|> updateTime ti
               :<|> deleteTime ti
             ))
  where u = userId tok

        listTimes :: App [Entity TimeEntry]
        listTimes = undefined
          -- runDb (selectList [TimeEntryUser ==. u] [])
          --           >>= return 
        
        createTime :: TimeEntry -> App (Key TimeEntry)
        createTime te = do
          a <- runDb $ do
            k <- insert te
            insert $ TimeEntryRel k u
            return k

          --return $ view (Control.Lens.from Key) a -- ^. ?
          return a
        
        getTime :: (Key TimeEntry) -> App (Entity TimeEntry)
        getTime i = undefined
          -- runDb (selectFirst [ (TimeEntryUser ==. u)
          --                              , (TimeEntryId ==. i)] [])
        
        updateTime :: (Key TimeEntry) -> TimeEntryAction -> App NoContent -- how to handle failure?
        updateTime i te = undefined
          -- do
          -- a <- runDb
          --   (E.updateCount $ \t -> do

          --     -- boiler plate!!! this can eventually be moved into a CRUD generalization
          --     E.set t [ TimeEntryClockin        E.=. (E.val $ clockin te)
          --             , TimeEntryClockout       E.=. (E.val $ clockout te)
          --             , TimeEntryDescription    E.=. (E.val $ description te)]

          --     -- check to make sure they own this row, security shouldn't be conflated here, but how then?
          --     E.where_ (t E.^. TimeEntryUser    E.==. (E.val u)
          --         E.&&. t E.^. TimeEntryId      E.==. (E.val i))
          --   )
          -- return . fromIntegral $ a
        
        deleteTime :: (Key TimeEntry) -> App NoContent
        deleteTime i = undefined
          -- runDb (delete i) >> return NoContent
        
timesServerT _ = throwAll err401

timesServerToHandler :: Config -> App :~> ExceptT ServantErr IO
timesServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

timesServer :: Config -> (AuthResult Token) -> Server TimesAPI
timesServer cfg u = enter (timesServerToHandler cfg) (timesServerT u)

