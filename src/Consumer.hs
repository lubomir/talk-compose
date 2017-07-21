{-# LANGUAGE OverloadedStrings #-}
module Consumer
    ( runConsumer
    ) where

import           Control.Monad               (forever, replicateM)
import           Data.Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.HashMap.Lazy           as HM
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as DB
import           System.ZMQ4

import Model
import Lib

type Topics = [BS.ByteString]
type MessageHandler = (BS.ByteString -> IO ())

subscribeTopics :: Subscriber a => Topics -> Socket a -> IO ()
subscribeTopics topics sock = mapM_ (subscribe sock) topics

ingestMessages :: Receiver a => Socket a -> MessageHandler -> IO ()
ingestMessages socket handler = forever $ receive socket >>= handler

asText :: Value -> Maybe T.Text
asText (String t) = Just t
asText _ = Nothing

extractCompose :: Value -> Maybe Compose
extractCompose (Object body) = do
    composeId <- HM.lookup "compose_id" body >>= asText
    location <- HM.lookup "location" body >>= asText
    status <- HM.lookup "status" body >>= asText
    return $ Compose composeId location status
extractCompose _ = Nothing

consume :: DB.ConnectionPool -> MessageHandler
consume pool msg =
    case decode (BSL.fromStrict msg) >>= extractCompose of
        Nothing -> putStrLn "Failed to process message"
        Just compose -> do
            print compose
            runDB pool $ do
                key <- DB.insertBy compose
                case key of
                    Left old -> DB.replace (DB.entityKey old) compose
                    Right _ -> return ()
            return ()

connectSocket :: DB.ConnectionPool -> String -> Topics -> Context -> IO ()
connectSocket pool endpoint topics context = withSocket context Sub $ \subscriber -> do
    putStrLn $ "Connecting to " ++ endpoint
    connect subscriber endpoint
    subscribeTopics topics subscriber
    ingestMessages subscriber (consume pool)

runConsumer :: IO ()
runConsumer = withDB $ \ pool -> 
    withContext $ connectSocket pool
                                "tcp://hub.fedoraproject.org:9940"
                                ["org.fedoraproject.prod.pungi.compose.status.change"]
