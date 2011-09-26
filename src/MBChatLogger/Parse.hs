{-# LANGUAGE OverloadedStrings #-}

module MBChatLogger.Parse (parseChatChannel) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E

import Data.Enumerator       (Iteratee)
import Data.Text             (Text, unpack)
import Data.Time             (readTime)
import Data.XML.Types        (Event)
import System.Locale         (defaultTimeLocale, iso8601DateFormat)
import Text.XML.Stream.Parse (content, many)

import MBChatLogger.Types
import MBChatLogger.XML

parseEvent :: Text -> Iteratee Event IO (Maybe IRCEvent)
parseEvent channel =
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
              doReadTime picoseconds
                | null picoseconds = reader (formatString False) . init
                | otherwise        = reader (formatString True) . (++ ("." ++ picoseconds)) . init
          in doReadTime picoseconds . unpack
        formatString False = iso8601DateFormat $ Just "%H:%M:%S"
        formatString True = iso8601DateFormat $ Just "%H:%M:%S%Q"

parseChatChannel :: Text -> Iteratee Event IO [IRCEvent]
parseChatChannel channel =
  reqTag rdf "RDF" $
    reqTag foaf "ChatChannel" $
      reqTag foaf "chatEventList" $
        reqTag rdf "Seq" $
          many (parseEvent channel)
