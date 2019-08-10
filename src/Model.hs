{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import           Data.List      (intercalate)
import           Data.Text      (Text, pack, unpack)
import qualified Data.Text as T
import           Data.Time
import           Text.Printf
import           Data.Monoid    ((<>))

import           Database.Persist.TH

share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
Compose
    composeId  Text
    location    Text
    status      Text
    release     Text
    version     Text
    date        Text
    type        Text
    respin      Int
    createdOn   UTCTime         default=now()
    modifiedOn  UTCTime         default=now()
    UniqueCompose composeId
    deriving Show
|]

composeDuration :: Compose -> Text
composeDuration Compose{..} =
    pack $ fmtDuration (diffUTCTime composeModifiedOn composeCreatedOn)

fmtDuration :: NominalDiffTime -> String
fmtDuration duration =
    intercalate ":" $ map (printf "%02d") values
  where
    total :: Integer
    total = round (realToFrac duration :: Double)
    values = [ total `div` 3600
             , (total `div` 60) `mod` 60
             , total `mod` 60
             ]

fmtTime :: UTCTime -> Text
fmtTime = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

parseComposeId :: Text -> (Text, Text, Text, Text, Int)
parseComposeId cid = case T.splitOn "-" (T.reverse cid) of
    (dr:version:release') ->
        let release = T.reverse (T.intercalate "-" release')
            (r', d') = T.breakOn "." dr
            (date, typ) = T.breakOn "." (T.init (T.reverse d'))
            respin = read $ unpack $ T.reverse r'
        in (release, T.reverse version, date, if T.null typ then "" else T.tail typ, respin)
    _ -> ("", "", "", "", 0)


getMainLogUrl :: Compose -> Text
getMainLogUrl c@Compose{..} = composeLocation <> "/../logs/global/pungi.global.log"
