{-|
Module      : Types
Description : Lib's types module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Types where

import           Control.Concurrent.STM
import           Data.Text
import           Web.Slack              hiding (Event)

data OutputMessage = SimpleMessage Text | RichMessage Attachment | NoMessage

data Command = Command { prefix  :: Char
                       , trigger :: Text
                       , args    :: [Text]
                       } deriving Show

data Event = Event { fullMessage :: Text
                   , user        :: UserId
                   , channel     :: ChannelId
                   , command     :: Command
                   }

data OutputResponse = OutputResponse { outputChannel :: TChan OutputResponse
                                     , event         :: Event
                                     , message       :: OutputMessage
                                     }

type BotInput = (Event, OutputResponse)
type BotAction = BotInput -> IO ()
