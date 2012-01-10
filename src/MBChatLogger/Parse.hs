{-# LANGUAGE OverloadedStrings #-}

module MBChatLogger.Parse (parseChatChannel, parseEvent) where

import Data.ByteString (ByteString)
import Data.Conduit ( Sink, ResourceThrow, (=$=), sequenceSink
                    , SequencedSinkResponse(..), Conduit)
import qualified Data.Conduit.List as CL
import Data.Text             (Text, unpack)
import Data.Time             (readTime)
import Data.XML.Types        (Event)
import System.Locale         (defaultTimeLocale, iso8601DateFormat)
import Text.XML.Stream.Parse (content, many, parseBytes, def)

import MBChatLogger.Types
import MBChatLogger.XML

parseEvent :: (ResourceThrow m) => Text -> Sink Event m (Maybe IRCEvent)
parseEvent channel = do
  nsTag rdf "li" $
    reqTagAttr foaf "chatEvent" (reqAttr rdf "ID") $ \rdfId -> do
      date <- parseTimestamp rdfId `fmap` reqTag dc "date" content
      desc <- reqTag dc "description" content
      name <- reqTag dc "creator" $
                reqTagAttr wn "Person" (reqAttr foaf "nick") $
                  \nick -> return nick
      many (nsTag dc "relation" $ return ())
      return $ Say name desc date channel
  where parseTimestamp timeString =
          let picoseconds = drop 10 (unpack timeString)
              reader = readTime defaultTimeLocale
              doReadTime pico
                | null pico = reader (formatString False) . init
                | otherwise = reader (formatString True) . (++ ("." ++ pico)) . init
          in doReadTime picoseconds . unpack
        formatString False = iso8601DateFormat $ Just "%H:%M:%S"
        formatString True = iso8601DateFormat $ Just "%H:%M:%S%Q"

parseChatChannel :: ResourceThrow m => Text -> Conduit ByteString m IRCEvent
parseChatChannel channelName =
    parseBytes def =$= sequenceSink () parsePrelude
  where parsePrelude _ = CL.drop 9 >> return (StartConduit eventConduit)
        eventConduit = sequenceSink () parse1
        parse1 _ = do ev <- parseEvent channelName
                      return $ case ev of
                        Just e -> Emit () [e]
                        Nothing -> Stop
