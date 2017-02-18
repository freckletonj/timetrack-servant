{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.User where

import Data.Aeson
import Data.Aeson.TH
import Data.Text              (Text, pack, unpack)
import Data.Text.Encoding     (decodeASCII)
import Data.Time              (UTCTime, getCurrentTime, addUTCTime)
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
                                   , get
                                   , toSqlKey
                                   , keyFromValues
                                   , insert
                                   , delete
                                   , update
                                   , replace
                                   , deleteWhere
                                   , ConnectionPool
                                   , ConnectionString
                                   , createPostgresqlPool
                                   , withPostgresqlPool
                                   , liftSqlPersistMPool
                                   , PersistValue (PersistText)
                                   , Entity
                                   , SqlBackend
                                   , selectList)

import Database.Persist.Sql (rawSql, SqlPersistT, unSingle, Single)
import Database.Persist.TH

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

-- import Control.Arrow (left)
import Data.Bifunctor
import Control.Monad.Trans.Either

import Models
import Config
import Lib

data UnsafeLogin = UnsafeLogin { email :: String
                               , clearPass :: String} deriving (Generic)
instance FromJSON UnsafeLogin



type LoginAPI = "login" :> ReqBody '[JSON] UnsafeLogin
                        :> Post '[JSON] (String)
              :<|> "new" :> ReqBody '[JSON] UnsafeLogin
                         :> Post '[JSON] (Key Login)

-- TODO: belongs in config
hashIterations = 12 -- 15 =~ 6 sec
tokenDuration = 60*60*24*30 -- one month, TODO: make revokable, expiration works, but can't be revoked that way

-- returnJwt :: JWTSettings -> String -> IO (Either Error BL.ByteString)
returnJwt jwts obj = do
  -- generate JWT
  now <- getCurrentTime
  let expiry = addUTCTime tokenDuration now
  etoken <- makeJWT obj
            jwts
            (Just expiry)
  return $ etoken
  
  -- case etoken of
  --   Left e -> error . show $ e
  --   Right v -> return . B.unpack . BL.toStrict $ v

loginServerT :: JWTSettings -> ServerT LoginAPI App
loginServerT jwts = login :<|> new
  where
    login :: UnsafeLogin -> App String
    login (UnsafeLogin email p) = do
      -- turn string into key
      
      case (keyFromValues [PersistText $ pack email]) of -- :: Either Text (Key Login)
        Left _ -> throwError err500
        Right k -> do
          mu <- (runDb (get k)) :: (App (Maybe Login))

          -- fetch user
          case maybe (Left $ pack "couldn't find user") Right mu of
            Left _ -> throwError err401
            Right (Login _ hp) ->

              -- validate password
              case (if validatePassword (B.pack p) (B.pack hp)
                then Right ()
                else Left "wrong pass") of
                Left _ -> throwError err401
                Right _ -> do

                  -- build etoken
                  etoken <- liftIO (returnJwt jwts (User email Nothing Nothing))
                  case bimap (pack . show) (B.unpack . BL.toStrict) etoken of
                    Left _ -> throwError err401
                    Right x -> return x

    new :: UnsafeLogin -> App (Key Login)
    new (UnsafeLogin e p) = do
      k <- runDb . insert $ (User e Nothing Nothing)
      hashed <- liftIO $ hashPassword hashIterations (B.pack p)
      k' <- runDb . insert $ (Login e $ B.unpack hashed)
      return k'

maybeToEither :: err -> Maybe a -> Either err a
maybeToEither = flip maybe Right . Left

--instance MonadIO (Either a) where
--instance MonadReader r m => MonadReader r (Either a) where
  
loginServerT' :: JWTSettings -> ServerT LoginAPI App
loginServerT' jwts = login :<|> new
  where
    new = undefined
    login :: UnsafeLogin -> App String
    login (UnsafeLogin email p) = eitherT (\_ -> throwError err401) return $

        hoistEither (keyFromValues [PersistText $ pack email])
        >>=
        (\k -> do
            mu <- fmap (maybeToEither "could'nt find em") (runDb (get k))
            hoistEither mu
        ) 
          
        >>= 
        (\(Login _ hp) -> hoistEither (if validatePassword (B.pack p) (B.pack hp)
                                       then Right ()
                                       else Left "wrong pass"))
        >>  (do
               etoken <- liftIO (returnJwt jwts (User email Nothing Nothing))
               bimapEitherT (pack . show) (B.unpack . BL.toStrict) (hoistEither etoken))
           
  
              


loginServerToHandler :: Config -> App :~> ExceptT ServantErr IO
loginServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

loginServer :: JWTSettings -> Config -> Server LoginAPI
loginServer jwts cfg = enter (loginServerToHandler cfg) (loginServerT jwts)



----------

type UserAPI = Get '[JSON] [Entity User]                 -- list all - todo: auth+role
                :<|> ReqBody '[JSON] User
                     :> Post '[JSON] (Key User)          -- add a new one
                :<|> Capture "userid" (Key User) :> 
                (
                  Get '[JSON] (Maybe User)               -- get one - todo: auth
                  :<|> ReqBody '[JSON] User
                       :> PutNoContent '[JSON] NoContent -- replace one - todo: auth
                  :<|> DeleteNoContent '[JSON] NoContent -- delete one - todo: auth
                )

userServerT :: ServerT UserAPI App
userServerT = listUsers
               :<|> putUser
               :<|> (\i ->
                       getUser i
                       :<|> updateUser i
                       :<|> deleteUser i
                    )
  where listUsers :: App [Entity User]
        listUsers = runDb (selectList [] []) >>= return
        putUser :: User -> App (Key User)
        putUser te = runDb (insert te) >>= return
        getUser :: (Key User) -> App (Maybe User)
        getUser i = runDb (get i) >>= return
        updateUser :: (Key User) -> User -> App NoContent
        updateUser i te = runDb (replace i te) >> return NoContent
        deleteUser :: (Key User) -> App NoContent
        deleteUser i = runDb (delete i) >> return NoContent

userServerToHandler :: Config -> App :~> ExceptT ServantErr IO
userServerToHandler cfg = Nat (flip runReaderT cfg . runApp)

userServer :: Config -> Server UserAPI
userServer cfg = enter (userServerToHandler cfg) userServerT


-- Reduction of the Login Problem
-- :: Int -> Either String Int
f = (\x -> if x>0          then Right x else Left "too small")
g = (\x -> if x<100        then Right x else Left "too big")
h = (\x -> if rem x 2 == 0 then Right x else Left "no odds")

thing :: (MonadIO m) => m String
thing = do
  a <- fmap read $ liftIO getLine
  case f a of
    Left x -> error x
    Right b ->
      case g b of
        Left x -> error x
        Right c ->
          case h c of
            Left x -> error x
            Right d ->
              return $ show . (+1) $ d
              
thing' :: (MonadIO m) => m String
thing' = do
  a <- fmap read $ liftIO getLine
  case (f a) >>= g >>= h of
    Left e -> error e
    Right x -> return $ show x
