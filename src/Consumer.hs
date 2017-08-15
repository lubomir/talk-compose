{-# LANGUAGE OverloadedStrings #-}
module Consumer
    ( runConsumer
    ) where

import           Control.Monad               (forever, replicateM, void)
import           Data.Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.HashMap.Lazy           as HM
import qualified Data.Text                   as T
import           Data.Time
import           Database.Persist            ((=.))
import qualified Database.Persist.Postgresql as DB
import           System.ZMQ4

import Model
import Lib

type Topic = BS.ByteString
type Msg = BS.ByteString
type MessageHandler = (Topic -> Msg -> IO ())

subscribeTopics :: Subscriber a => [Topic] -> Socket a -> IO ()
subscribeTopics topics sock = mapM_ (subscribe sock) topics

ingestMessages :: Receiver a => Socket a -> MessageHandler -> IO ()
ingestMessages socket handler = forever $ do
    topic <- receive socket
    msg <- receive socket
    handler topic msg

asText :: Value -> Maybe T.Text
asText (String t) = Just t
asText _ = Nothing

asObject :: Value -> Maybe (HM.HashMap T.Text Value)
asObject (Object obj) = Just obj
asObject _ = Nothing

extractCompose :: UTCTime -> Value -> Maybe Compose
extractCompose now (Object body) = do
    msg <- HM.lookup "msg" body >>= asObject
    composeId <- HM.lookup "compose_id" msg >>= asText
    location <- HM.lookup "location" msg >>= asText
    status <- HM.lookup "status" msg >>= asText
    return $ Compose composeId location status now now
extractCompose _ _ = Nothing

consume :: DB.ConnectionPool -> MessageHandler
consume pool topic msg = do
    now <- getCurrentTime
    case decode (BSL.fromStrict msg) >>= extractCompose now of
        Nothing -> BSC.putStrLn $ "Failed to process message from " `BSC.append` topic `BSC.append` ": " `BSC.append` msg
        Just compose ->
          void $ runDB pool $ DB.upsert compose [ ComposeStatus =. composeStatus compose
                                                , ComposeModifiedOn =. now]

connectSocket :: DB.ConnectionPool -> String -> [Topic] -> Context -> IO ()
connectSocket pool endpoint topics context = withSocket context Sub $ \subscriber -> do
    putStrLn $ "Connecting to " ++ endpoint
    connect subscriber endpoint
    subscribeTopics topics subscriber
    ingestMessages subscriber (consume pool)

runDB :: DB.ConnectionPool -> DB.SqlPersistT IO a -> IO a
runDB pool q = DB.runSqlPool q pool

withDB :: (DB.ConnectionPool -> IO a) -> IO a
withDB worker = do
    env <- getEnvironment
    pool <- getPool env
    runDB pool $ DB.runMigration migrateAll
    worker pool

runConsumer :: IO ()
runConsumer = do
    putStrLn "Connecting to database..."
    withDB $ \ pool -> do
        putStrLn "Connecting to ZMQ..."
        withContext $ connectSocket pool
                                    "tcp://hub.fedoraproject.org:9940"
                                    ["org.fedoraproject.prod.pungi.compose.status.change"]
