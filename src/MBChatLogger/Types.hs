module MBChatLogger.Types
       (IRCEvent(..)
       ) where

import Data.Text       (Text)
import Data.Time.Clock (UTCTime)

--------------------------------------------------------------------------------
-- | An event that has happened in an IRC channel.
data IRCEvent =
  -- | A person has said something to everyone in the channel.
  Say { -- | A ID unique for that day.
        evId :: Text
        -- | The name of the user who said the message.
      , sayUser :: Text
        -- | The message that was said.
      , sayBody :: Text
        -- | The timestamp the message was said.
      , sayTimestamp :: UTCTime
  }
