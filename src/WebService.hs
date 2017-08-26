{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module WebService where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), asks)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist.Postgresql as DB
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, Options(..), ScottyT, scottyOptsT, middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setFdCacheDuration, setPort)
import           System.Environment          (lookupEnv)
import Data.Default.Class
import Network.Wai (Middleware)

import Model
import Lib

data Config = Config { environment :: Environment
                     , pool :: DB.ConnectionPool
                     }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
                  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text

type Action = ActionT Error ConfigM ()

type Handler = ScottyT Error ConfigM ()

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
    return Config{..}

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
