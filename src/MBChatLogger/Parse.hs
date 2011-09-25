{-# LANGUAGE OverloadedStrings #-}

module MBChatLogger.Parse (parseChatChannel) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E

import Data.Enumerator       (Iteratee)
import Data.Time             (readTime)
import Data.XML.Types        (Event)
import System.Locale         (defaultTimeLocale, iso8601DateFormat)
import Text.XML.Stream.Parse (content, many)

import MBChatLogger.Types
import MBChatLogger.XML

parseEvent :: Iteratee Event IO (Maybe IRCEvent)
parseEvent =
  nsTag rdf "li" $
    reqTagAttr foaf "chatEvent" (reqAttr rdf "ID") $ \rdfId -> do
      date <- parseTimestamp `fmap` reqTag dc "date" content
      desc <- reqTag dc "description" content
      name <- reqTag dc "creator" $
                reqTagAttr wn "Person" (reqAttr foaf "nick") $
                  \nick -> return nick
      many (nsTag dc "relation" $ return ())
      return $ Say rdfId name desc date
  where parseTimestamp =
          readTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ") .
            BS.unpack . E.encodeUtf8

parseChatChannel :: Iteratee Event IO [IRCEvent]
parseChatChannel =
  reqTag rdf "RDF" $
    reqTag foaf "ChatChannel" $
      reqTag foaf "chatEventList" $
        reqTag rdf "Seq" $
          many parseEvent
