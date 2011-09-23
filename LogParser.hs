 {-# LANGUAGE OverloadedStrings #-}
import Data.Enumerator (run_)
import qualified Data.Enumerator.List as EL
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as L
import Data.Text (unpack, Text)
import Text.XML.Stream.Parse
import Data.XML.Types
import Data.Aeson
import Network.HTTP.Enumerator

data IRCEvent = Say { evId :: Text, sayUser :: Text, sayBody :: Text }
     deriving Show

rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
foaf = "http://xmlns.com/foaf/0.1/"
dc = "http://purl.org/dc/elements/1.1/"
wn = "http://xmlns.com/wordnet/1.6/"

instance ToJSON IRCEvent where
  toJSON (Say id' user body) = object [ "user" .= user
                                      , "body" .= body
                                      , "id"   .= id'
                                      ]

nsTagIgnore ns tname f = tagName (Name { nameLocalName = tname
                                    , nameNamespace = Just ns
                                    , namePrefix = Nothing
                                    }) (ignoreAttrs) $ \_ -> f

nsTag ns tname = tagName (Name { nameLocalName = tname
                                 , nameNamespace = Just ns
                                 , namePrefix = Nothing
                                 })

requireNsAttr ns name = requireAttr $ Name { nameLocalName = name
                                           , nameNamespace = Just ns
                                           , namePrefix = Nothing
                                           }
                                 
parseEvent =
  nsTagIgnore rdf "li" $
    force "chatEvent" $ nsTag foaf "chatEvent" (requireNsAttr rdf "ID") $ \rdfId -> do
      date <- nsTagIgnore dc "date" content
      desc <- force "Need body" $ nsTagIgnore dc "description" content
      name <- force "Need creator" $ nsTagIgnore dc "creator" $
                force "" $ nsTag wn "Person" (requireNsAttr foaf "nick") $
                  \nick -> return nick
      nsTagIgnore dc "relation" $ return ()
      return $ Say rdfId name desc

parseChatChannel =
  force "Start with RDF" $ nsTagIgnore rdf "RDF" $
    force "ChatChannel" $ nsTagIgnore foaf "ChatChannel" $
      force "chatEventList" $ nsTagIgnore foaf "chatEventList" $
        force "Seq" $ nsTagIgnore rdf "Seq" $
          many (parseEvent)
     
main = do
  events <- parseFile_ def "2011-09-22.rdf" $ parseChatChannel
  index `mapM_` events
  where
    index ev = do
      r <- (parseUrl "http://127.0.0.1:9200/irc/message")
      (withManager $ httpLbs r { method = "POST", requestBody = RequestBodyLBS (encode $ toJSON ev) })
