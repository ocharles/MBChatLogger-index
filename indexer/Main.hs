{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (catch)
import Data.List
import Data.Text (Text, unpack)
import Control.Exception
import Network.HTTP.Enumerator
import Text.XML.Stream.Parse (parseLBS_, def)
import Text.Printf

import MBChatLogger.Parse
import MBChatLogger.Index

main :: IO ()
main = uncurry indexChannel `mapM_` [("musicbrainz", 2003)
                                    ,("musicbrainz-devel", 2009)
                                    ]

indexChannel :: Text -> Int -> IO ()
indexChannel channel from = do
  (\day -> catch (indexDay day) handleError) `mapM_` dates
  where indexDay day = do
          putStrLn ("Indexing " ++ unpack channel ++ day)
          log <- simpleHttp ("http://chatlogs.musicbrainz.org/" ++ (unpack channel) ++ "/" ++ day ++ ".rdf")
          parseLBS_ def log (parseChatChannel channel) >>= mapM_ index
        formatDate y m d =
          let year = show y
              pad = printf "%02d"
              parts = [ [show y]
                      , [show y, pad (m::Integer)]
                      , [show y, pad (m::Integer), pad d]
                      ]
          in intercalate "/" $ intercalate "-" `map` parts
        dates = [formatDate y m d | y <- [from..2011], m <- [3..12], d <- [6..31], not $ invalidDate y m d ]
        invalidDate y m d = (y, m, d) `elem` [(2003, 3, 6)]

handleError :: HttpException -> IO ()
handleError = print
