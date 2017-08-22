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

import           Control.Monad  (mzero)
import           Data.Aeson
import           Data.Text      (Text, pack, unpack)
import           Data.Text.Lazy (toStrict)
import           Data.Time

import           Database.Persist.TH

-- TODO: parse compose ID before storing in database
share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
Compose
    composeId  Text
    location    Text
    status      Text
    createdOn   UTCTime         default=now()
    modifiedOn  UTCTime         default=now()
    UniqueCompose composeId
    deriving Show
|]

composeDuration :: Compose -> Text
composeDuration Compose{..}
    | composeCreatedOn == composeModifiedOn = ""
    | otherwise = pack $ show $ diffUTCTime composeModifiedOn composeCreatedOn

fmtTime :: UTCTime -> Text
fmtTime = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
