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
import           Data.Text
import           Network.HTTP.Client
import           Network.Wreq
-- import qualified Network.Wreq           as W
import           System.Environment
import           Types

envFail :: String -> IO String
envFail var = envDefault (error var ++ " not set") var

envWarn :: String -> IO String
envWarn var = do
  getVar <- lookupEnv var
  unless (isJust getVar) $ print failureStr
  return $ fromMaybe failureStr getVar
  where failureStr = var ++ " not set"

envMaybe :: String -> IO (Maybe String)
envMaybe = lookupEnv

envGuard :: String -> IO String
envGuard var = do
  getVar <- lookupEnv var
  guard $ isJust getVar
  return $ fromJust getVar

envDefault :: String -> String -> IO String
envDefault def var = fromMaybe def <$> lookupEnv var

safeGetUrl :: Text -> Maybe Text -> Maybe Text -> IO (Either Text (Response LBS.ByteString))
safeGetUrl url (Just login) (Just pass) = do
  let opts = defaults & auth ?~ basicAuth (BSC.pack $ unpack login) (BSC.pack $ unpack pass)
                      -- & W.checkStatus .~ (Just $ \_ _ _ -> Nothing)
  (Right <$> getWith opts (unpack url)) `E.catch` handler
  where
    handler :: HttpException -> IO (Either Text (Response LBS.ByteString))
    handler _ = return $ Left $ pack "Error fetching HTTP data."
    -- handler (StatusCodeException s _ _) = do
    --   return $ Left $ BSC.unpack (s ^. statusMessage)
safeGetUrl _ _ _ = return $ Left $ pack "Error: Invalid invocation."

slackWriter :: OutputResponse -> OutputMessage -> IO ()
slackWriter resp msg = atomically $ writeTChan (outputChannel resp) resp { message = msg }
