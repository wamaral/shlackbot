module Main where

import           Data.Maybe         (fromMaybe)
import           Lib
import           System.Environment (lookupEnv)
import           Web.Slack

myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig { _slackApiToken = apiToken }

main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set")
              <$> lookupEnv "SLACK_API_TOKEN"
  runSlack (myConfig apiToken) shlackbot
