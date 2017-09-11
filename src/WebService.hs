{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module WebService where

import           Control.Monad.Base
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader,
                                                       ReaderT (..), asks)
import           Control.Monad.Trans.Class            (MonadTrans, lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8                as BS
import           Data.Default.Class
import qualified Data.Text                            as T
import           Data.Text.Lazy                       (Text)
import qualified Database.Persist.Postgresql          as DB
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       setFdCacheDuration,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Environment                   (lookupEnv)
import qualified Web.JWT                              as JWT
import           Web.Scotty.Cookie
import           Web.Scotty.Trans                     (ActionT, Options (..),
                                                       ScottyT, middleware,
                                                       scottyOptsT)

import Model
import Lib

data Config = Config { environment :: Environment
                     , pool :: DB.ConnectionPool
                     , secret :: JWT.Secret
                     }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
                  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config, MonadBase IO, MonadThrow)

instance MonadBaseControl IO ConfigM where
    type StM ConfigM a = a
    -- TODO is this needed?

instance MonadResource ConfigM where
    -- TODO is this needed?

type Error = Text

type Action = ActionT Error ConfigM ()

type Handler = ScottyT Error ConfigM ()

type CookieName = T.Text

runDB :: (MonadTrans t, MonadIO (t ConfigM))
      => DB.SqlPersistT IO a
      -> t ConfigM a
runDB q = do
    p <- lift (asks pool)
    liftIO (DB.runSqlPool q p)

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production  = logStdout
loggingM Test        = id

getPort :: IO (Maybe Int)
getPort = fmap read <$> lookupEnv "PORT"

getSettings :: Environment -> IO Settings
getSettings e = do
    let cache = if e == Development then setFdCacheDuration 0 else id
    portSetter <- maybe id setPort <$> getPort
    return $ portSetter $ cache defaultSettings

getOptions :: Environment -> IO Options
getOptions e = do
    s <- getSettings e
    return def { settings = s
               , verbose = case e of Development -> 1
                                     _           -> 0
               }

getConfig :: IO Config
getConfig = do
    environment <- getEnvironment
    pool <- getPool environment
    secret <- getSecret
    return Config{..}

getSecret :: IO JWT.Secret
getSecret = JWT.binarySecret . maybe defaultSecret BS.pack <$> lookupEnv "TALK_COMPOSE_ENV"
  where defaultSecret = "CHANGE_ME_IN_PRODUCTION"

application :: Environment -> ScottyT Error ConfigM () -> ScottyT Error ConfigM ()
application e app = do
    middleware (loggingM e)
    app

runApplication :: Config -> ScottyT Error ConfigM () -> IO ()
runApplication c app = do
    o <- getOptions (environment c)
    let r m = runReaderT (runConfigM m) c
    scottyOptsT o r (application (environment c) app)

runService :: ScottyT Error ConfigM () -> IO ()
runService app = do
    c <- getConfig
    DB.runSqlPool (DB.runMigration migrateAll) (pool c)
    runApplication c app

loginCookie :: CookieName
loginCookie = "LOGIN"

optionalUser :: ActionT Error ConfigM (Maybe JWT.StringOrURI)
optionalUser = do
    mjson <- getCookie loginCookie
    secret <- lift (asks secret)
    case mjson of
        Nothing -> return Nothing
        Just json -> case JWT.decodeAndVerifySignature secret json of
            Nothing -> return Nothing
            Just jwt -> return (JWT.sub (JWT.claims jwt))
            -- TODO add verification of expiration
