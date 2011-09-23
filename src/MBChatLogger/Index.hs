{-# LANGUAGE OverloadedStrings #-}

module MBChatLogger.Index where

import Data.Aeson              (ToJSON (..), encode, object, (.=))
import Network.HTTP.Enumerator (parseUrl, withManager, httpLbs, method
                               ,requestBody, RequestBody(..), Response)

import MBChatLogger.Types

instance ToJSON IRCEvent where
  toJSON (Say id' user body) = object [ "user" .= user
                                      , "body" .= body
                                      , "id"   .= id'
                                      ]

index :: IRCEvent -> IO Response
index ev = do
  r <- (parseUrl "http://127.0.0.1:9200/irc/message")
  withManager $
    httpLbs r { method = "POST"
              , requestBody = RequestBodyLBS (encode $ toJSON ev)
              }
