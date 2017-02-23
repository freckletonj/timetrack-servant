{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Api.TimeEntry where

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

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Aeson.Lens
import Control.Lens hiding ((.=), set, (^.))
import Control.Lens.TH
import GHC.Generics           (Generic)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Database.Esqueleto as E

import Models
import Config
import Lib
import Api.Login

-- sample data: 
-- {"clockin": "2013-10-17T09:42:49.007Z",
-- "description": "first success"}

--------------------------------------------------
-- Annoying Data Munging

-- TimeEntrys that go over the Wire
--   they're just missing the `user` foreign key
--   and of course the TimeEntryID which Persistent
--   normally handles anyways through `Entity`s

data TimeEntryW = TimeEntryW { clockin :: UTCTime
                             , clockout :: Maybe UTCTime
                             , description :: String}
                  deriving (Show, Generic, ToJSON, FromJSON)

timeEntryW :: TimeEntry -> TimeEntryW
timeEntryW TimeEntry{..} = TimeEntryW {
  clockin       = timeEntryClockin
  , clockout    = timeEntryClockout
  , description = timeEntryDescription
  }

timeEntry :: (Key User) -> TimeEntryW -> TimeEntry
timeEntry fk TimeEntryW{..} = TimeEntry {
  timeEntryUser          = fk
  , timeEntryClockin     = clockin
  , timeEntryClockout    = clockout
  , timeEntryDescription = description
  }


--------------------------------------------------
-- Api

type TimesAPI =
  -- List All
  Get '[JSON] [Entity TimeEntry]

  -- Create New
  :<|> ReqBody '[JSON] TimeEntryW
  :> Post '[JSON] (Key TimeEntry)

  :<|> Capture "TimeEntryId" (Key TimeEntry) :>
  (

    -- Get One
    Get '[JSON] (Maybe (Entity TimeEntry))

    -- Update One
    :<|> ReqBody '[JSON] TimeEntryW
    :> Put '[JSON] Int

    -- Delete One
    :<|> Delete '[JSON] NoContent
  )

  
timesServerT :: AuthResult Token -> ServerT TimesAPI App
timesServerT (Authenticated tok)  =
  listTimes
  :<|> createTime
  :<|> (\ti ->
           getTime ti
           :<|> updateTime ti
           :<|> deleteTime ti
       )
  where u = userId tok

        listTimes :: App [Entity TimeEntry]
        listTimes = runDb (selectList [TimeEntryUser ==. u] [])
                    >>= return
        
        createTime :: TimeEntryW -> App (Key TimeEntry)
        createTime tew = runDb (insert (timeEntry u tew)) >>= return
        
        getTime :: (Key TimeEntry) -> App (Maybe (Entity TimeEntry))
        getTime i = runDb (selectFirst [ (TimeEntryUser ==. u)
                                       , (TimeEntryId ==. i)] [])
                    -- >>= return
        
        updateTime :: (Key TimeEntry) -> TimeEntryW -> App Int
        updateTime i te = do
          a <- runDb
            (E.updateCount $ \t -> do

              -- boiler plate!!!
              E.set t [ TimeEntryClockin        E.=. (E.val $ clockin te)
                      , TimeEntryClockout       E.=. (E.val $ clockout te)
                      , TimeEntryDescription    E.=. (E.val $ description te)]

              -- check to make sure they own this row
              E.where_ (t E.^. TimeEntryUser    E.==. (E.val u)
                  E.&&. t E.^. TimeEntryId      E.==. (E.val i))
            )
          return . fromIntegral $ a
        
        deleteTime :: (Key TimeEntry) -> App NoContent
        deleteTime i = runDb (delete i) >> return NoContent
        
timesServerT _ = throwAll err401

timesServerToHandler :: Config -> App :~> ExceptT ServantErr IO
timesServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

timesServer :: Config -> (AuthResult Token) -> Server TimesAPI
timesServer cfg u = enter (timesServerToHandler cfg) (timesServerT u)

