{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Database.Persist.Postgresql as DB
import Data.Monoid ((<>))
import Web.Scotty.Trans
import Control.Monad.IO.Class (liftIO)
import Lucid

import Model
import WebService

defaultTemplate :: Html a -> Html a
defaultTemplate content = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        link_ [href_ "/static/style.css", rel_ "stylesheet"]
    body_ $ div_ [class_ "content"] content

template :: Html a -> Action
template content = do
    setHeader "Content-Type" "text/html"
    text . renderText . defaultTemplate $ content

composeBox :: DB.Entity Compose -> Html ()
composeBox (DB.Entity _ c) =
    a_ [href_ (composeComposeId c), class_ " compose ", class_ (composeStatus c)] $ do
        span_ [class_ "composeId"] (toHtml $ composeComposeId c)
        span_ [class_ "status"] (toHtml $ composeStatus c)

composePage :: Compose -> Html ()
composePage Compose{..} = do
    h1_ $ toHtml composeComposeId
    ul_ $ do
        li_ $ a_ [href_ composeLocation] "View"
        li_ $ "First heard of: " <> toHtml (fmtTime composeCreatedOn)
        li_ $ "Last heard of: " <> toHtml (fmtTime composeModifiedOn)
    h2_ "Comments"

main :: IO ()
main = runService $ do
    get "/" $ do
        composes <- runDB $ DB.selectList [] [DB.Desc ComposeId]
        template $ mconcat $ map composeBox composes

    get "/:id" $ do
        cid <- param "id"
        mcompose <- runDB $ DB.selectList [ComposeComposeId DB.==. cid] []
        case mcompose of
            [DB.Entity _ compose] -> template $ composePage compose
            _ -> next

    get "/static/style.css" $ do
        setHeader "Content-Type" "text/css"
        file "static/style.css"
