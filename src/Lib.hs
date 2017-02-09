{-# LANGUAGE LambdaCase #-}

{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
    ( shlackbot
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text              as T
import           Parser                 as P
import qualified Plugin.Jira            as Jira
import           Text.Megaparsec        (parseMaybe)
import           Types
import           Web.Slack              hiding (Event)
import           Web.Slack.Handle       (SlackHandle, withSlackHandle)
import           Web.Slack.Monad        (monadicToHandled)

pluginHandlers :: [(Event, OutputResponse) -> IO ()]
pluginHandlers = [ Jira.respond ]

shlackbot :: Slack ()
shlackbot = do
  me <- _selfUserId . _slackSelf <$> getSession
  conf <- getConfig

  incoming <- liftIO $ atomically newBroadcastTChan
  outgoing <- liftIO $ atomically newTChan

  _ <- mapM (liftIO . runPlugin incoming) pluginHandlers
  _ <- liftIO $ withSlackHandle conf $ relayMessage outgoing

  forever $ getNextEvent >>= \case
    Message cid (UserComment uid) msg _ _ _ | uid /= me -> do
      let parsedCmd = parseMaybe P.commandParser msg
      case parsedCmd of
        Just c -> do
          liftIO $ print c
          let evt = Event msg uid cid c
          let response = OutputResponse outgoing evt NoMessage
          liftIO $ atomically $ writeTChan incoming (evt, response)
        Nothing -> return ()
    _ -> return ()

runPlugin :: TChan (Event, OutputResponse) -> ((Event, OutputResponse) -> IO ()) -> IO ThreadId
runPlugin chan f = do
  myChan <- atomically $ dupTChan chan
  forkIO $ forever $ do
    msg <- atomically $ readTChan myChan
    f msg

relayMessage :: TChan OutputResponse -> SlackHandle -> IO ThreadId
relayMessage chan h = do
  forkIO $ forever $ do
    resp <- atomically $ readTChan chan
    monadicToHandled (publishMessage resp) h

publishMessage :: OutputResponse -> Slack ()
publishMessage resp = do
  let cid = channel $ event resp
  case (message resp) of
    SimpleMessage txt -> slackSendMsg cid txt []
    RichMessage rmsg  -> slackSendMsg cid "" [rmsg]
    NoMessage         -> return ()

slackSendMsg :: ChannelId -> T.Text -> [Attachment] -> Slack ()
slackSendMsg cid msg att = do
  send <- sendRichMessage cid msg att
  either (liftIO . putStrLn . T.unpack) return send
