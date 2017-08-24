{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Monad.IO.Class      (liftIO)
import           Data.List
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import qualified Database.Persist.Postgresql as DB
import           Lucid
import           Web.Scotty.Trans

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

formatType :: Compose -> Html ()
formatType Compose{..}
    | composeType == "" = ""
    | otherwise = "." <> toHtml composeType

composeBox :: Compose -> Html ()
composeBox c =
    a_ [href_ (composeComposeId c), class_ " compose ", class_ (composeStatus c)] $ do
        span_ [class_ "composeId"] $ do
            toHtml (composeDate c )
            formatType c
            "."
            toHtml (show $ composeRespin c)
        span_ [class_ "status"] (toHtml $ composeStatus c)
        span_ [class_ "duration"] (toHtml $ composeDuration c)

composeRow :: ([(Text, Text)], [Compose]) -> Html ()
composeRow ((release, version):_, composes) = do
    h2_ $ toHtml release <> " " <> toHtml version
    div_ [class_ "row"] $ mconcat $ map composeBox composes


getRelease :: Compose -> Text
getRelease Compose{..} =
    let (prefix, date) = T.breakOnEnd "-" composeComposeId
    in T.dropEnd 1 prefix

composePage :: Compose -> Html ()
composePage Compose{..} = do
    h1_ $ toHtml composeComposeId
    ul_ $ do
        li_ $ a_ [href_ composeLocation] "View"
        li_ $ "First heard of: " <> toHtml (fmtTime composeCreatedOn)
        li_ $ "Last heard of: " <> toHtml (fmtTime composeModifiedOn)
    h2_ "Comments"
    p_ "To be done"

main :: IO ()
main = runService $ do
    get "/" $ do
        -- Select only composes updated in the last 10 days.
        let diff = -10 * 24 * 3600
        offset <- addUTCTime diff <$> liftIO getCurrentTime
        composes <- runDB $ DB.selectList [ComposeModifiedOn DB.>. offset] [DB.Desc ComposeComposeId]
        template $ mconcat
                 $ map (composeRow . unzip)
                 $ groupBy (\x y -> fst x == fst y)
                 $ map ((\x -> ((composeRelease x, composeVersion x), x)) . DB.entityVal) composes

    get "/:id" $ do
        cid <- param "id"
        mcompose <- runDB $ DB.selectList [ComposeComposeId DB.==. cid] []
        case mcompose of
            [DB.Entity _ compose] -> template $ composePage compose
            _ -> next

    get "/static/style.css" $ do
        setHeader "Content-Type" "text/css"
        file "static/style.css"
