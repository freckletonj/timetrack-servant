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
import Data.Bool              (bool)
import Data.Text              (Text, pack, unpack)
import Data.Time              (getCurrentTime)
import Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import Data.Typeable          (Typeable)
import Data.UUID
import Debug.Trace            (trace)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger   (runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.Except   (ExceptT, MonadError)

import Data.Aeson
import Data.Aeson.Types       (Parser, parseMaybe)
import Data.Aeson.Lens
import Control.Lens hiding    ((.=), set, (^.), from)
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
import Models.Util
import Config
import Lib
import Api.Login

{-
TODO: IMPORTANT GENERALIZATION

----------

CRUD and relatives belong in their own module

actionToMethod should be a class method so that the server for CRUD can be generalized

class Updatable a where
  actionToMethod :: ModelAction -> [Update Model]

note: an ActionModel is like the model, but all Fields become Maybe Fields
      I would think that (Maybe x) fields should become (Maybe (Maybe x))
      so that the field can be reset to Nothing if the client wishes

we need CRUD, and CRUD-with-FKs, possibly they can be related

CRUD-with-FKs should just need passed in a Model, and a ModelRelations

ModelAction could be generated as well, TH?

-}

--------------------------------------------------
-- CRUD

type CRUD db act = ReqBody '[JSON] db :> Post '[JSON] (Key db)
                   :<|> Capture "id" (Key db) :> (
  Get '[JSON] db
  :<|> ReqBody '[JSON] act :> Put '[JSON] String -- Should prob be
                                                 -- NoContent, but
                                                 -- cljs-ajax thinks
                                                 -- those are errors
  :<|> Delete '[JSON] String)
  
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
    actClockin :: Maybe CUTCTime
  , actClockout :: Maybe CUTCTime
  , actDescription :: Maybe String
  } deriving (Show)

instance FromJSON TimeEntryAction where
  parseJSON (Object o) = TimeEntryAction
    <$> o .:? "clockin"
    <*> o .:? "clockout"
    <*> o .:? "description"

--actionToUpdates :: TimeEntryAction -> [P.Update TimeEntry]
actionToUpdates TimeEntryAction{..} = updateClockin
                                      ++ updateClockout
                                      ++ updateDescription
  where
    updateClockin     = maybe [] (\x -> [TimeEntryClockin     =. val x])       actClockin
    updateClockout    = maybe [] (\x -> [TimeEntryClockout    =. val (Just x)]) actClockout
    updateDescription = maybe [] (\x -> [TimeEntryDescription =. val x])   actDescription

    
--------------------------------------------------
-- Api

-- TODO: i can probably simplify the server implementation
type TimesAPI = Get '[JSON] [Entity TimeEntry]
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
        listTimes = runDb (select $
                          Database.Esqueleto.from $ \(t, r) -> do
                              where_ (t ^. TimeEntryId ==. r ^. TimeEntryRelTime
                                  &&. r ^. TimeEntryRelUser ==. val u)
                              return t)
                    >>= return
                          
        createTime :: TimeEntry -> App (Key TimeEntry)
        createTime te = (runDb $ do
                            k <- insert te            -- insert time
                            insert $ TimeEntryRel k u -- insert relation to user
                            return k)
                        >>= return
        
        getTime :: (Key TimeEntry) -> App TimeEntry
        getTime i = runDb (get i) >>= maybe (throwError err404) return -- TODO: not row-level secure
        
        updateTime :: (Key TimeEntry) -> TimeEntryAction -> App String -- how to handle failure?
        updateTime i act = (runDb $ updateCount $ \te -> do
                              set te $ actionToUpdates act
                              where_ (te ^. TimeEntryId ==. val i))
                           >>= bool (throwError err404) (return "") . (> 0)
        
        deleteTime :: (Key TimeEntry) -> App String
        deleteTime i = (runDb $ do
                           -- TODO, something with cascading may be more efficient here
                           --       deleteCascade exists, but doesn't count (IIRC),
                           --       so I wouldn't be able to return an appropriate code
                           -- delete relation first
                           c' <- deleteCount $ from (\ tr -> where_ (tr ^. TimeEntryRelTime ==. val i)) 
                           c <- deleteCount $ from (\ te -> where_ (te ^. TimeEntryId ==. val i))
                           return $ c+c')
                       >>= bool (throwError err404) (return "") . (==2)
        
timesServerT _ = throwAll err401

timesServerToHandler :: Config -> App :~> ExceptT ServantErr IO
timesServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

timesServer :: Config -> (AuthResult Token) -> Server TimesAPI
timesServer cfg u = enter (timesServerToHandler cfg) (timesServerT u)

