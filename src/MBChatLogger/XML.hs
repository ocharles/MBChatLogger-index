{-# LANGUAGE OverloadedStrings #-}

-- | Various xml-enumerator parser combinators for convenience.
module MBChatLogger.XML
       ( -- * Namespaces
         rdf, wn, foaf, dc
         -- * Parser Combinators
       , nsTag, nsTagAttr
       , reqTag, reqTagAttr
       , reqAttr
       ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E
import           Data.Enumerator            (Iteratee)
import           Data.Text                  (Text)
import           Data.XML.Types             (Name (..), Event)
import           Text.XML.Stream.Parse      (force, tagName, ignoreAttrs
                                            ,requireAttr, AttrParser)


--------------------------------------------------------------------------------
-- Namespace aliases
rdf :: Text
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

foaf :: Text
foaf = "http://xmlns.com/foaf/0.1/"

dc :: Text
dc = "http://purl.org/dc/elements/1.1/"

wn :: Text
wn = "http://xmlns.com/wordnet/1.6/"

--------------------------------------------------------------------------------
-- | Require a tag to be present, but ignore its attributes.
reqTag :: Monad m
       => Text
       -> Text
       -> Iteratee Event m c
       -> Iteratee Event m c
reqTag ns tname = force errorMsg . nsTag ns tname
  where errorMsg = (BS.unpack $ E.encodeUtf8 tname) ++ " is required"

--------------------------------------------------------------------------------
-- | Require a tag to be present, and use a specific attribute parser.
reqTagAttr :: Monad m
           => Text
           -> Text
           -> AttrParser b
           -> (b -> Iteratee Event m c)
           -> Iteratee Event m c
reqTagAttr ns tname a f = force errorMsg $ nsTagAttr ns tname a f
  where errorMsg = (BS.unpack $ E.encodeUtf8 tname) ++ " is required"

--------------------------------------------------------------------------------
-- | Optional tag parser, that ignore all attributes.
nsTag :: Monad m
      => Text -> Text -> Iteratee Event m b
      -> Iteratee Event m (Maybe b)
nsTag ns tname f = tagName (Name { nameLocalName = tname
                                       , nameNamespace = Just ns
                                       , namePrefix = Nothing
                                       }) (ignoreAttrs) $ \_ -> f

--------------------------------------------------------------------------------
-- | Optional tag parser, that can parse attributes
nsTagAttr :: Monad m
          => Text -> Text -> AttrParser a -> (a -> Iteratee Event m b)
          -> Iteratee Event m (Maybe b)
nsTagAttr ns tname = tagName (Name { nameLocalName = tname
                                   , nameNamespace = Just ns
                                   , namePrefix = Nothing
                                   })

--------------------------------------------------------------------------------
-- | An attribute parser that requires an attribute in a certain namespace is
-- present.
reqAttr :: Text -> Text -> AttrParser Text
reqAttr ns name = requireAttr $ Name { nameLocalName = name
                                     , nameNamespace = Just ns
                                     , namePrefix = Nothing
                                     }
