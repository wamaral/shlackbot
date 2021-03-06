{-|
Module      : Util
Description : Lib's utils module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Util where

import           Control.Concurrent.STM
import qualified Control.Exception      as E
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy   as LBS
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Client
import           Network.Wreq
-- import qualified Network.Wreq           as W
import           System.Environment
import           Types
import           Web.Slack

slackUser :: UserId -> Text
slackUser (Id uid) = T.concat ["<@", uid, ">"]

slackSimpleQuote :: Text -> Text
slackSimpleQuote txt = T.concat ["`", txt, "`"]

envFail :: String -> IO String
envFail var = envDefault (error var ++ " not set") var

envWarn :: String -> IO String
envWarn var = do
  getVar <- lookupEnv var
  unless (isJust getVar) $ print failureStr
  pure $ fromMaybe failureStr getVar
  where failureStr = var ++ " not set"

envMaybe :: String -> IO (Maybe String)
envMaybe = lookupEnv

envGuard :: String -> IO String
envGuard var = do
  getVar <- lookupEnv var
  guard $ isJust getVar
  pure $ fromJust getVar

envDefault :: String -> String -> IO String
envDefault def var = fromMaybe def <$> lookupEnv var

safeGetUrl :: Text -> Maybe Text -> Maybe Text -> IO (Either Text (Response LBS.ByteString))
safeGetUrl url (Just login) (Just pass) = do
  let opts = defaults & auth ?~ basicAuth (BSC.pack $ T.unpack login) (BSC.pack $ T.unpack pass)
                      -- & W.checkStatus .~ (Just $ \_ _ _ -> Nothing)
  (Right <$> getWith opts (T.unpack url)) `E.catch` handler
  where
    handler :: HttpException -> IO (Either Text (Response LBS.ByteString))
    handler _ = pure $ Left $ T.pack "Error fetching HTTP data."
    -- handler (StatusCodeException s _ _) = do
    --   pure $ Left $ BSC.unpack (s ^. statusMessage)
safeGetUrl _ _ _ = pure $ Left $ T.pack "Error: Invalid invocation."

slackWriter :: OutputResponse -> OutputMessage -> IO ()
slackWriter resp msg = atomically $ writeTChan (outputChannel resp) resp { message = msg }
