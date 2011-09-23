{-# LANGUAGE OverloadedStrings #-}

module MBChatLogger.Parse (parseChatChannel) where

import Data.Enumerator       (Iteratee)
import Data.XML.Types        (Event)
import Text.XML.Stream.Parse (content, many)

import MBChatLogger.Types
import MBChatLogger.XML

parseEvent :: Iteratee Event IO (Maybe IRCEvent)
parseEvent =
  nsTag rdf "li" $
    reqTagAttr foaf "chatEvent" (reqAttr rdf "ID") $ \rdfId -> do
      date <- reqTag dc "date" content
      desc <- reqTag dc "description" content
      name <- reqTag dc "creator" $
                reqTagAttr wn "Person" (reqAttr foaf "nick") $
                  \nick -> return nick
      nsTag dc "relation" $ return ()
      return $ Say rdfId name desc

parseChatChannel :: Iteratee Event IO [IRCEvent]
parseChatChannel =
  reqTag rdf "RDF" $
    reqTag foaf "ChatChannel" $
      reqTag foaf "chatEventList" $
        reqTag rdf "Seq" $
          many parseEvent
