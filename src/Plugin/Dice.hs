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

respond :: BotResponse
respond (evt, resp) = maybe (pure ()) parseCmd (command evt)
  where
    doRoll c = roll (arguments c) (slackUser $ user evt) resp
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

diceError :: Show a => a -> String
diceError = concat . intersperse ": " . tail . lines . show

roll :: [Text] -> Text -> OutputResponse -> IO ()
roll args username resp = do
  let diceRequest = diceOrDefault args
  diceResult <- rollEm diceRequest
  let diceMessage = (slackSimpleQuote $ T.pack $ either diceError id diceResult)
  slackWriter resp $ SimpleMessage $ T.concat [username, " ", (slackSimpleQuote $ T.pack diceRequest), " => ", diceMessage]
