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
module Api
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
                                   , SqlBackend
                                   , selectList)

import Database.Persist.Sql (rawSql, SqlPersistT, unSingle, Single)
import Database.Persist.TH

import Models
import Config


import Lib


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
      dbcfg = (Config connPool env)
      
      jwtCfg = defaultJWTSettings myKey
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
    
  run 8081 app

-- outputs something like the following, before starting the server:
-- curl -H "Authorization: Bearer "eyJhbGciOiJIUzI1NiJ9.eyJkYXQiOnsiZW1haWwiOiJjaGFyaXphcmQuYXdlc29tZUBob3RtYWlsLmNvbSIsIm5hbWUiOiJjaGFyaXphcmQifX0.t-VlSuSZi6l67uguOEZXDBkcMkxMvDx-f8sRVMPy-O8"" localhost:8080/name -v




