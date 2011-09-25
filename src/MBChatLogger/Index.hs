{-# LANGUAGE OverloadedStrings #-}

module MBChatLogger.Index
       ( index, search
       ) where

import Control.Applicative     ((<*>), (<$>))
import Data.Aeson              (ToJSON (..), object, (.=), FromJSON (..), (.:)
                               ,Value (..))
import qualified Data.Text as T
import Data.Time               (formatTime)
import Search.ElasticSearch    (Document (..), ElasticSearch, localServer
                               ,indexDocument, DocumentType (..)
                               ,SearchResults)
import System.Locale           (iso8601DateFormat, defaultTimeLocale)

import MBChatLogger.Types

import qualified Search.ElasticSearch as ES

instance ToJSON IRCEvent where
  toJSON (Say user body time channel) =
    object [ "user" .= user
           , "body" .= body
           , "time" .= time
           , "channel" .= channel
           ]

instance FromJSON IRCEvent where
  parseJSON (Object o) = Say <$> o .: "user"
                             <*> o .: "body"
                             <*> o .: "time"
                             <*> o .: "channel"

instance Document IRCEvent where
  documentKey (Say user _ time _) = user `T.append` (T.pack $ formatter time)
    where formatter = formatTime defaultTimeLocale fmt
          fmt = iso8601DateFormat $ Just "%H:%M:%S %Q"
  documentType = DocumentType "event"

server :: ElasticSearch
server = localServer

index :: IRCEvent -> IO ()
index = indexDocument server "irc"

search :: Integer -> T.Text -> IO (SearchResults IRCEvent)
search = ES.search server "irc"
