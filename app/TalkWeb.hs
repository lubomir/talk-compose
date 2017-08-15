{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Database.Persist.Postgresql as DB
import Web.Scotty.Trans
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text

import Model
import WebService

main :: IO ()
main = runService $ do
    get "/" $ do
        setHeader "Content-Type" "text/html"
        composes <- runDB $ DB.selectList [] [DB.Desc ComposeId]
        text $ renderHtml $ $(shamletFile "templates/index.hamlet")

    get "/:id" $ do
        cid <- param "id"
        setHeader "Content-Type" "text/html"
        mcompose <- runDB $ DB.selectList [ComposeComposeId DB.==. cid] []
        case mcompose of
            [DB.Entity _ compose] -> text $ renderHtml $ $(shamletFile "templates/compose.hamlet")
            _ -> text "404"

    get "/static/style.css" $ do
        setHeader "Content-Type" "text/css"
        file "static/style.css"
