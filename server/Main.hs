{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Snap.Core
import Snap.Http.Server
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Text.Blaze
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Text.Lazy           (toStrict)
import Text.Blaze.Renderer.Text (renderHtml)
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (traverse)
import Data.Monoid (mempty)
import Data.ByteString.Char8 (unpack)
import Data.List (genericLength)
import Data.Maybe

import MBChatLogger.Index (search)
import MBChatLogger.Types
import Search.ElasticSearch (getResults, result, totalHits)

handler :: Snap ()
handler = do
  queryInput <- getParam "q"
  let query = E.decodeUtf8 `fmap` queryInput
  offset <- maybe 0 (read . unpack) `fmap` getParam "offset"
  results <- (liftIO . search offset) `traverse` query
  output $ H.html $ do
    H.head $ do
      H.title "MBChatLogger Search"
      H.style $ toHtml css
    H.body $ do
      H.h1 "Search"
      H.p "Forget what you were talking about? Don't worry, it happens to me all the time."
      H.p "Supported search fields are..."
      H.ul $
        H.li (H.code "user:" >> " Search by nickname")
      H.form ! A.method "GET" $
        H.p $ do
          H.label ! A.for "search-query" $ "Query:"
          H.input ! A.name "q" ! A.id "search-query"
          H.input ! A.type_ "submit" ! A.value "Go"
      case results of
        Just res -> do
          H.h1 "Results"
          mapM_ (displayResult . result) $ getResults res
          when (totalHits res > offset + genericLength (getResults res)) $
            H.p ! A.class_ "more" $ do
              "But wait, "
              H.a ! A.href (toValue ("/?q=" ++ T.unpack (fromJust query) ++ "&offset=" ++ show (offset + genericLength (getResults res)))) $ "there's more..."
        Nothing -> mempty
  where displayResult ev =
          H.p ! A.class_ "message" $ do
            H.span ! A.class_ "nick" $ toHtml $ sayUser ev
            H.span ! A.class_ "body" $ toHtml $ sayBody ev
        css = unlines [ "body { font-family: sans-serif; font-size: 12px }"
                      , ".nick { width: 8em; text-align: right; float: left; display: block; font-weight: bold; }"
                      , ".body { margin-left: 11.5em; display: block }"
                      , "p.message { clear: both; background: #f3f3f3; padding: 1em; border-radius: 5px }"
                      , "p.more { text-align: center; font-size: 18pt; border-top: 1px solid #eee; padding: 1em; color: #555; }"
                      ]

output :: Html -- ^ The 'Html' to display.
       -> Snap ()
output = writeText . toStrict . renderHtml

main :: IO ()
main = quickHttpServe handler
