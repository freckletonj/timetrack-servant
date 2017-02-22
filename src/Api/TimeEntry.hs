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
-- ( (==.)
--                                    , runSqlPool
--                                    , get
--                                    , getBy
--                                    , insert
--                                    , delete
--                                    , update
--                                    , replace
--                                    , deleteWhere
--                                    , ConnectionPool
--                                    , ConnectionString
--                                    , createPostgresqlPool
--                                    , withPostgresqlPool
--                                    , liftSqlPersistMPool
--                                    , Entity
--                                    , SqlBackend
--                                    , selectList)

-- import Database.Persist.Sql -- (rawSql, SqlPersistT, unSingle, Single)
import Database.Persist.TH
--import Database.Esqueleto
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
--   normally handles anyways

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




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -- THIS STUFF IS JUST FOR REFERENCE, AND ISNT REALLY USED ANYWHERE
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------



-- --------------------------------------------------
-- -- Auth API
-- type Protected
--     = "email"  :> Get '[JSON] String
-- -- :<|> "email" :> Get '[JSON] String
-- protected :: AuthResult User -> Server Protected
-- protected (Authenticated user) = return (userEmail user)
-- protected _ = throwAll err401


-- {-

-- type Unprotected =
--  "login"
--      :> ReqBody '[JSON] Login
--      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] String)
--   :<|> Raw

-- unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
-- unprotected cs jwts = checkCreds cs jwts
--                  :<|> serveDirectory "example/static" 


-- -- for static assets
-- -}

-- -- --------------------------------------------------
-- -- -- Misc API

-- -- type MiscAPI = "nowIO" :> Get '[JSON] UTCTime
-- --           :<|> "error" :> Get '[JSON] String
-- --           :<|> "printIO" :> Get '[JSON] NoContent

-- -- miscServer :: Server MiscAPI
-- -- miscServer = now
-- --              :<|> error
-- --              :<|> printer
-- --   where
-- --         now :: Handler UTCTime
-- --         now = liftIO getCurrentTime >>= return
-- --         error :: Handler String
-- --         error = throwError err404 { errBody = "a dinosaur ate the server" }
-- --         printer :: Handler NoContent
-- --         printer = liftIO $ putStrLn "printed!" >> return NoContent

-- -- --------------------------------------------------
-- -- -- ReaderMonad API

-- -- type ReaderAPI = Get '[JSON] UTCTime
-- -- readerServerT :: ServerT ReaderAPI App
-- -- readerServerT = f
-- --   where
-- --     f :: App UTCTime
-- --     f = runDb selNow >>= return . unSingle . head

-- -- readerAPI :: Proxy ReaderAPI
-- -- readerAPI = Proxy

-- -- readerToHandler :: Config -> App :~> ExceptT ServantErr IO
-- -- readerToHandler cfg = Nat (flip runReaderT cfg . runApp)

-- -- readerServer :: Config -> Server ReaderAPI
-- -- readerServer  cfg = enter (readerToHandler cfg) readerServerT

-- -- selNow :: MonadIO m => ReaderT SqlBackend m [Single UTCTime]
-- -- selNow = rawSql "select now()" []

-- -- allTimeEntries :: App [Entity TimeEntry]
-- -- allTimeEntries = runDb (selectList [] [])

