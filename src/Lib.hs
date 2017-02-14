{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- NOTE : for Reader Monad see : https://github.com/haskell-servant/servant/blob/29f94a64408d6a7815dbe43fd06e8b372b3230e9/doc/tutorial/Server.lhs#L1104
--

module Lib
    ( startApp
    ) where

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

import GHC.Generics           (Generic)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import Database.Persist.Postgresql (runSqlPool
                                   , ConnectionPool
                                   , ConnectionString
                                   , createPostgresqlPool
                                   , withPostgresqlPool
                                   , liftSqlPersistMPool
                                   , Entity
                                   , Single
                                   , SqlBackend
                                   , selectList
                                   , unSingle)
import Database.Persist.Sql (rawSql, SqlPersistT)
import Database.Persist.TH

import Models

--------------------------------------------------
-- Types

-- types imported from Models

times :: [TimeEntry]
times = [ TimeEntry (posixSecondsToUTCTime 0) (Just $ posixSecondsToUTCTime 1) "first"
        , TimeEntry (posixSecondsToUTCTime 2) Nothing "second"]

--------------------------------------------------
-- Misc API

type MiscAPI = "now" :> Get '[JSON] UTCTime
          :<|> "error" :> Get '[JSON] String
          :<|> "print" :> Get '[JSON] NoContent

miscServer :: Server MiscAPI
miscServer = now
             :<|> error
             :<|> printer
  where
        now :: Handler UTCTime
        now = liftIO getCurrentTime >>= return
        error :: Handler String
        error = throwError err404 { errBody = "a dinosaur ate the server" }
        printer :: Handler NoContent
        printer = liftIO $ putStrLn "printed!" >> return NoContent

--------------------------------------------------
-- Reader API

type ReaderAPI = Get '[JSON] UTCTime
readerServerT :: ServerT ReaderAPI App -- (ReaderT (IO a) Config)
readerServerT = f
  where
    f :: App UTCTime
    f = do
      x <- runDb selNow
      return $ unSingle $ head x


readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

-- readerToHandler' :: Config -> forall a. Reader Config a -> Handler a
-- readerToHandler' cfg r = return (runReader r cfg)
readerToHandler :: Config -> App :~> ExceptT ServantErr IO
readerToHandler cfg = Nat (flip runReaderT cfg . runApp)

readerServer :: Config -> Server ReaderAPI
readerServer  cfg = enter (readerToHandler cfg) readerServerT

--instance ToJSON Dbtime where
selNow :: MonadIO m => ReaderT SqlBackend m [Single UTCTime]
selNow = rawSql "select now()" []

selNow' :: App [Single UTCTime]
selNow' = runDb (rawSql "select now()" [])

-- belongs in Models.hs ?
runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

-- runDb' ::  ReaderT SqlBackend IO a -> IO a
-- runDb' query = do
--   pool <- asks getPool
--   runSqlPool query pool
  
--------------------------------------------------
-- Time API

type TimesAPI = Get '[JSON] [TimeEntry]
                -- :<|> Capture "timeid" Int :> Get '[JSON] TimeEntry

timesServer :: Server TimesAPI
timesServer = listTimes
              -- :<|> getTime
  where listTimes :: Handler [TimeEntry]
        listTimes = return times

        -- getTime :: Int -> Handler TimeEntry
        -- getTime i = return $ head $ filter (\t -> timeid t == i) times

--------------------------------------------------
-- Auth API

type Protected
    = "name"  :> Get '[JSON] String
 :<|> "email" :> Get '[JSON] String

type Unprotected =
 "login"
     :> ReqBody '[JSON] Login
     :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] String)
  :<|> Raw

protected :: AuthResult User -> Server Protected
protected (Authenticated user) = return (name user) :<|> return (email user)
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = checkCreds cs jwts
                 :<|> serveDirectory "example/static" 

--------------------------------------------------
-- servant-auth

data User = User { name :: String, email :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Login = Login { username :: String, password :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

checkCreds :: CookieSettings -> JWTSettings -> Login
  -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
checkCreds cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
   let usr = User "Ali Baba" "ali@email.com"
   mcookie <- liftIO $ makeCookie cookieSettings jwtSettings usr
   case mcookie of
     Nothing     -> throwError err401
     Just cookie -> return $ addHeader cookie $ show cookie
checkCreds _ _ _ = throwError err401

--------------------------------------------------
-- RUN

type API auths = MiscAPI
                 :<|> "times" :> TimesAPI
                 :<|> "read" :> ReaderAPI
                 :<|> (Auth auths User :> Protected)
                 :<|> Unprotected
                 

server :: Config -> CookieSettings -> JWTSettings -> Server (API auths)
server cfg cs jwts = miscServer
                 :<|> timesServer
                 :<|> readerServer cfg
                 :<|> protected
                 :<|> unprotected cs jwts


data Env = Development
         | Testing
         | Production

data Config = Config { getPool :: ConnectionPool
                     , getEnv :: Env }

connStr :: ConnectionString
connStr = "host=localhost dbname=timetrack-test user=timetrack-test password=test port=5432"

makePool :: Env -> IO ConnectionPool
makePool Development = runStdoutLoggingT $ createPostgresqlPool connStr 8
makePool Testing     = undefined
makePool Production  = undefined



newtype App a = App { runApp :: ReaderT Config (ExceptT ServantErr IO) a
                    } deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

allTimeEntries :: App [Entity TimeEntry]
allTimeEntries = runDb (selectList [] [])

-- userApp :: a -> Config -> Application
-- userApp jwtCfg cfg = serve (Proxy :: Proxy (API '[JWT])) (appToServer jwtCfg cfg)

-- appToServer :: a -> Config -> Server (API '[JWT])
-- appToServer jwtCfg cfg = enter (convertApp cfg) (server defaultCookieSettings jwtCfg)

-- convertApp :: Config -> App :~> ExceptT ServantErr IO
-- convertApp cfg = Nat (flip runReaderT cfg . runApp)

-- for static assets
files :: Application
files = serveDirectory "assets"


startApp :: IO ()
startApp = do
  -- This Db IO happens outside of servant
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ liftSqlPersistMPool $ do
    x <- selNow
    liftIO (print x)

  -- Db for use _inside_ servant
  connPool <- runStdoutLoggingT $ createPostgresqlPool connStr 8
  
  myKey <- generateKey
  let env = Development
      jwtCfg = defaultJWTSettings myKey
      dbcfg = (Config connPool env)
      cfg = defaultCookieSettings
         :. jwtCfg
         :. EmptyContext
        
      api = Proxy :: Proxy (API '[JWT])

      app :: Application
      app = serveWithContext api
                             cfg
                             (server dbcfg defaultCookieSettings jwtCfg)

  -- generate a valid test token
  etoken <- makeJWT (User "charizard" "pokemon.awesome@hotmail.com") jwtCfg Nothing
  case etoken of
    Left e -> putStrLn $ "Error generating token: " ++ show e 
    Right v -> putStrLn $ "try this: " ++ "curl -H \"Authorization: Bearer " ++ show v ++ "\" localhost:8080/name -v"
    
  run 8080 app

-- outputs something like the following, before starting the server:
-- curl -H "Authorization: Bearer "eyJhbGciOiJIUzI1NiJ9.eyJkYXQiOnsiZW1haWwiOiJjaGFyaXphcmQuYXdlc29tZUBob3RtYWlsLmNvbSIsIm5hbWUiOiJjaGFyaXphcmQifX0.t-VlSuSZi6l67uguOEZXDBkcMkxMvDx-f8sRVMPy-O8"" localhost:8080/name -v



