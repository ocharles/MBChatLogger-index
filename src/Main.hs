{-# LANGUAGE OverloadedStrings #-}
import Text.XML.Stream.Parse (parseFile_, def)

import MBChatLogger.Parse
import MBChatLogger.Index

main :: IO ()
main = parseFile_ def "2011-09-22.rdf" parseChatChannel >>= mapM_ index
