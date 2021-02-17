{-# LANGUAGE OverloadedStrings #-}
module LogParser where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import Model

type Matcher = T.Text -> Maybe T.Text

downloadMainLog :: Manager -> T.Text -> IO T.Text
downloadMainLog manager url = do
    request <- parseRequest $ T.unpack url
    response <- httpLbs request manager
    return $ T.strip $ decodeUtf8 $ LBS.toStrict $ responseBody response

extractValue :: T.Text -> T.Text -> Maybe T.Text
extractValue needle line =
    let (prefix, suffix) = T.breakOn needle line
    in if T.null suffix then Nothing else Just (T.drop (T.length needle) suffix)

extractPungiVersion :: T.Text -> Maybe T.Text
extractPungiVersion = extractValue "Pungi version: "

extractHostname :: T.Text -> Maybe T.Text
extractHostname = extractValue "Host: "


getUpdates :: T.Text -> [(EntityField Compose T.Text, T.Text)]
getUpdates log = findData [] matchers (T.lines log)
  where
    matchers = [ (extractPungiVersion, ComposePungiVersion)
               , (extractHostname, ComposeHostname)
               ]

findData :: [(a, T.Text)] -> [(Matcher, a)] -> [T.Text] -> [(a, T.Text)]
findData res _ [] = res
findData res [] _ = res
findData res ms (l:ls) = let (ms', result) = applyMatchers l res ms
                       in findData result ms' ls

applyMatchers :: T.Text
              -> [(a, T.Text)]
              -> [(Matcher, a)]
              -> ([(Matcher, a)], [(a, T.Text)])
applyMatchers line = go []
  where
    go remaining res [] = (remaining, res)
    go remaining res ((m, f):ms) = case m line of
        Nothing -> go ((m, f):remaining) res ms
        Just v  -> go remaining ((f, v):res) ms
