{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import           Control.Monad  (mzero)
import           Data.Aeson
import           Data.Text      (Text, pack, unpack)
import           Data.Text.Lazy (toStrict)
import           Data.Time

import           Database.Persist.TH

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

