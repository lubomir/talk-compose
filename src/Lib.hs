{-# LANGUAGE OverloadedStrings #-}
module Lib where


import qualified Database.Persist.Postgresql as DB
import System.Environment (lookupEnv)
import Control.Monad.Logger (runStdoutLoggingT, runNoLoggingT)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BS8


import Model


data Environment = Production
                 | Development
                 | Test
                 deriving (Eq, Show, Read)


getConnectionSize :: Environment -> Int
getConnectionSize Production = 2
getConnectionSize _          = 1

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e =
    maybe defConnStr BS8.pack <$> lookupEnv "DATABASE_CONNECTION"
  where
    defConnStr = getDefaultConnectionString e

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString e =
    let n = case e of
                Production  -> "talk_compose_production"
                Development -> "talk_compose_development"
                Test        -> "talk_compose_test"
    in createConnectionString
        [ ("host", "localhost")
        , ("port", "5432")
        , ("user", "postgres")
        , ("dbname", n)
        ]

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l = encodeUtf8 (T.unwords (map f l))
  where f (k, v) = T.concat [k, "=", v]


getEnvironment :: IO Environment
getEnvironment = maybe Development read <$> lookupEnv "TALK_COMPOSE_ENV"

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
    s <- getConnectionString e
    let n = getConnectionSize e
    case e of
        Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
        Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
        Test -> runNoLoggingT (DB.createPostgresqlPool s n)

runDB :: DB.ConnectionPool -> DB.SqlPersistT IO a -> IO a
runDB pool q = DB.runSqlPool q pool

withDB :: (DB.ConnectionPool -> IO a) -> IO a
withDB worker = do
    env <- getEnvironment
    pool <- getPool env
    runDB pool $ DB.runMigration migrateAll
    worker pool
