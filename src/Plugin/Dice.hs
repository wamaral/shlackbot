-- Taken from https://github.com/lambdabot/lambdabot/blob/master/lambdabot-novelty-plugins/src/Lambdabot/Plugin/Novelty/Dice.hs
module Plugin.Dice
  ( respond
  , help
  ) where

import           Data.List        (intersperse)
import           Data.Maybe       (maybe)
import           Data.Random.Dice (rollEm)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Types
import           Util
import           Web.Slack        hiding (lines)

respond :: BotResponse
respond (evt, resp) = maybe (pure ()) parseCmd (command evt)
  where
    doRoll c = roll (arguments c) (user evt) resp
    parseCmd cmd = case trigger cmd of
      "roll" -> doRoll cmd
      "dice" -> doRoll cmd
      "help" -> help (arguments cmd) resp
      _      -> pure ()

help :: [Text] -> OutputResponse -> IO ()
help args resp = case args of
  []       -> say
  ["roll"] -> say
  ["dice"] -> say
  _        -> pure ()
  where say = slackWriter resp $ QuotedSimpleMessage "!roll|dice <expr> :: Rolls random dice, <expr> is in the form 3d10+2, defaults to 1d6"

diceOrDefault :: [Text] -> String
diceOrDefault [] = "1d6"
diceOrDefault x  = T.unpack $ T.concat x

roll :: [Text] -> UserId -> OutputResponse -> IO ()
roll args uid resp = do
  result <- rollEm $ diceOrDefault args
  write $ either showErr id result
  where
    showErr = concat . intersperse ": " . tail . lines . show
    prefx = T.concat $ (slackUser uid) : " " : args
    write str = slackWriter resp $ SimpleMessage $ T.concat [prefx, " ", slackSimpleQuote $ T.pack str]
