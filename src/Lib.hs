{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

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

import GHC.Generics           (Generic)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

--------------------------------------------------
-- Types

data TimeEntry = TimeEntry
  { timeid :: Int
  , clockin :: UTCTime
  , description :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''TimeEntry)

times :: [TimeEntry]
times = [ TimeEntry 1 (posixSecondsToUTCTime 0) "first"
        , TimeEntry 2 (posixSecondsToUTCTime 2) "second"]

--------------------------------------------------
-- Misc API

type MiscAPI = "now" :> Get '[JSON] UTCTime
          :<|> "error" :> Get '[JSON] String

miscServer :: Server MiscAPI
miscServer = now
             :<|> error
  where
        now :: Handler UTCTime
        now = liftIO getCurrentTime >>= return
        error :: Handler String
        error = throwError err404 { errBody = "a dinosaur ate the server" }
        
--------------------------------------------------
-- Time API

type TimesAPI = Get '[JSON] [TimeEntry]
           :<|> Capture "timeid" Int :> Get '[JSON] TimeEntry

timesServer :: Server TimesAPI
timesServer = listTimes
              :<|> getTime
  where listTimes :: Handler [TimeEntry]
        listTimes = return times

        getTime :: Int -> Handler TimeEntry
        getTime i = return $ head $ filter (\t -> timeid t == i) times

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
                 :<|> (Auth auths User :> Protected)
                 :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts = miscServer
                 :<|> timesServer
                 :<|> protected
                 :<|> unprotected cs jwts

startApp :: IO ()
startApp = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])

  -- generate a valid test token
  etoken <- makeJWT (User "charizard" "pokemon.awesome@hotmail.com") jwtCfg Nothing
  case etoken of
    Left e -> putStrLn $ "Error generating token: " ++ show e 
    Right v -> putStrLn $ "try this: " ++ "curl -H \"Authorization: Bearer " ++ show v ++ "\" localhost:8080/name -v"
    
  run 8080 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)

-- outputs something like the following, before starting the server:
-- curl -H "Authorization: Bearer "eyJhbGciOiJIUzI1NiJ9.eyJkYXQiOnsiZW1haWwiOiJjaGFyaXphcmQuYXdlc29tZUBob3RtYWlsLmNvbSIsIm5hbWUiOiJjaGFyaXphcmQifX0.t-VlSuSZi6l67uguOEZXDBkcMkxMvDx-f8sRVMPy-O8"" localhost:8080/name -v



