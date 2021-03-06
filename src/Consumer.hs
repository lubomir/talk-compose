{-# LANGUAGE OverloadedStrings #-}
module Consumer
    ( runConsumer
    ) where

import           Control.Monad               (forever, void)
import           Data.Aeson
import qualified Data.ByteString             as BS
-- import qualified Data.ByteString.Lazy        as BSL
import           Control.Concurrent.Async    (race_)
import           Control.Concurrent.STM
import qualified Data.HashMap.Lazy           as HM
import qualified Data.Text                   as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.Persist            ((=.))
import qualified Database.Persist.Postgresql as DB
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.ZMQ4                 (Context, Receiver, Socket,
                                              Sub (..), Subscriber, connect,
                                              receive, subscribe, withContext,
                                              withSocket)

import Model
import Lib
import LogParser

type Topic = BS.ByteString
type MessageHandler = (Topic -> Message -> IO ())

data Crypto = X509 | GPG deriving (Show, Eq)

instance FromJSON Crypto where
    parseJSON = withText "String" $ \txt -> case txt of
        "x509" -> return X509
        "gpg" -> return GPG
        _ -> fail "Unknown crypto type"

data Message = Message { msgTopic :: T.Text
                       , msgTimestamp :: UTCTime
                       , msgMsgId :: T.Text
                       , msgCrypto :: Crypto
                       --, msgCert :: Maybe BSL.ByteString
                       --, msgSignature :: Maybe BSL.ByteString
                       , msgI :: Int
                       , msgUsername :: T.Text
                       , msgMsg :: Value
                       } deriving (Show, Eq)

instance FromJSON Message where
    parseJSON = withObject "Object" $ \obj -> Message
        <$> obj .: "topic"
        <*> (posixSecondsToUTCTime . fromInteger <$> obj .: "timestamp")
        <*> obj .: "msg_id"
        <*> obj .: "crypto"
        -- <*> obj .:? "certificate"
        -- <*> obj .:? "signature"
        <*> obj .: "i"
        <*> obj .: "username"
        <*> obj .: "msg"

-- TODO
--  * download certificate to a known location
validateX509 :: Message -> Bool
validateX509 msg = True

validateGPG :: Message -> Bool
validateGPG msg = False

validate :: Message -> Bool
validate msg = case msgCrypto msg of
    X509 -> validateX509 msg
    GPG  -> validateGPG msg

subscribeTopics :: Subscriber a => [Topic] -> Socket a -> IO ()
subscribeTopics topics sock = mapM_ (subscribe sock) topics

ingestMessages :: Receiver a => Socket a -> MessageHandler -> IO ()
ingestMessages socket handler = forever $ do
    topic <- receive socket
    rawmsg <- receive socket
    case eitherDecodeStrict rawmsg of
        Left err -> putStrLn $ "Failed to decode message:" ++ err
        Right msg -> if validate msg
                        then handler topic msg
                        else putStrLn $ "Invalid signature: " ++ show msg

asText :: Value -> Maybe T.Text
asText (String t) = Just t
asText _ = Nothing

extractCompose :: UTCTime -> Value -> Maybe Compose
extractCompose now (Object msg) = do
    composeId <- HM.lookup "compose_id" msg >>= asText
    location <- HM.lookup "location" msg >>= asText
    status <- HM.lookup "status" msg >>= asText
    let (release, version, date, typ, respin) = parseComposeId composeId
    return $ Compose composeId location status release version date typ respin "" "" now now
extractCompose _ _ = Nothing

consume :: DB.ConnectionPool -> TQueue Compose -> MessageHandler
consume pool queue _ msg = do
    now <- getCurrentTime
    case extractCompose now (msgMsg msg) of
        Nothing -> putStrLn $ "Failed to process message: " ++ show msg
        Just compose -> do
          atomically $ writeTQueue queue compose
          void $ runDB pool $ DB.upsert compose [ ComposeStatus =. composeStatus compose
                                                , ComposeModifiedOn =. now]

connectSocket :: DB.ConnectionPool -> TQueue Compose -> String -> [Topic] -> Context -> IO ()
connectSocket pool queue endpoint topics context = withSocket context Sub $ \subscriber -> do
    putStrLn $ "Connecting to " ++ endpoint
    connect subscriber endpoint
    subscribeTopics topics subscriber
    ingestMessages subscriber (consume pool queue)

runDB :: DB.ConnectionPool -> DB.SqlPersistT IO a -> IO a
runDB pool q = DB.runSqlPool q pool

withDB :: (DB.ConnectionPool -> IO a) -> IO a
withDB worker = do
    env <- getEnvironment
    pool <- getPool env
    runDB pool $ DB.runMigration migrateAll
    worker pool



consumerThread :: TQueue Compose -> IO ()
consumerThread queue = do
    putStrLn "Consumer is connecting to database..."
    withDB $ \ pool -> do
        putStrLn "Connecting to ZMQ..."
        withContext $ connectSocket pool
                                    queue
                                    "tcp://hub.fedoraproject.org:9940"
                                    ["org.fedoraproject.prod.pungi.compose.status.change"]

updaterThread :: TQueue Compose -> IO ()
updaterThread queue = do
    putStrLn "Updater is connecting to database..."
    withDB $ \ pool -> do
        putStrLn "Starting worker thread..."
        manager <- newManager tlsManagerSettings
        logProcessor manager pool
  where
    logProcessor :: Manager -> DB.ConnectionPool -> IO ()
    logProcessor manager pool = forever $ do
        compose <- atomically $ readTQueue queue
        putStrLn $ "Updating compose " ++ show (composeComposeId compose)
        case getMainLogUrl compose of
            Nothing -> putStrLn $ "Invalid URL. Skipping..."
            Just url -> do
                log <- downloadMainLog manager url
                let updates = [f DB.=. v | (f, v) <- getUpdates log]
                void $ runDB pool $ DB.upsert compose updates

runConsumer :: IO ()
runConsumer = do
    queue <- atomically newTQueue
    race_ (consumerThread queue) (updaterThread queue)
