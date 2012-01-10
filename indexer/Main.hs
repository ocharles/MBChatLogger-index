{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (catch)
import Data.Conduit (($=), ($$))
import qualified Data.Conduit.List as CL
import Data.List
import Data.Text (Text, unpack)
import Control.Exception
import Network.HTTP.Conduit
import Text.Printf

import MBChatLogger.Parse
import MBChatLogger.Index

main :: IO ()
main = uncurry indexChannel `mapM_` [("musicbrainz", 2012)
                                    ,("musicbrainz-devel", 2012)
                                    ]

indexChannel :: Text -> Int -> IO ()
indexChannel channel from = do
  (\day -> catch (indexDay day) handleError) `mapM_` dates
  where
    indexDay day = do
      putStrLn ("Indexing " ++ unpack channel ++ day)
      putStrLn ("http://chatlogs.musicbrainz.org/" ++
                            (unpack channel) ++ "/" ++ day ++ ".rdf")
      request <- parseUrl $ "http://chatlogs.musicbrainz.org/" ++
                            (unpack channel) ++ "/" ++ day ++ ".rdf"
      withManager $ \m -> do
        res <- http request m
        responseBody res $= parseChatChannel channel $$ CL.mapM_ index
    formatDate y m d =
      let pad = printf "%02d"
          parts = [ [show y]
                  , [show y, pad (m::Integer)]
                  , [show y, pad (m::Integer), pad d]
                  ]
      in intercalate "/" $ intercalate "-" `map` parts
    dates = [ formatDate y m d
            | y <- [from..2012]
            , m <- [1..12]
            , d <- [1..31]
            , not $ invalidDate y m d
            ]
    invalidDate y m d = (y, m, d) `elem` [(2003, 3, 6)]

handleError :: HttpException -> IO ()
handleError = print
