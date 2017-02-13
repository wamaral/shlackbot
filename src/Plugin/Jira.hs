{-# LANGUAGE RecordWildCards #-}

module Plugin.Jira where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Maybe
import           Data.Text           hiding (head, map)
-- import           Data.Text.Lazy          (toStrict)
-- import           Data.Text.Lazy.Encoding
import           Network.HTTP.Types
import           Network.Wreq
import           Types
import qualified Types               as T
import           Util
import           Web.Slack

data Issue = Issue { issueKey     :: Text
                   , summary      :: Text
                   , description  :: Maybe Text
                   , priorityName :: Maybe Text
                   , priorityIcon :: Maybe Text
                   , issueStatus  :: Maybe Text
                   , assigneeName :: Maybe Text
                   , creatorName  :: Maybe Text
                   , issueUrl     :: Maybe Text
                   , commentCount :: Maybe Integer
                   } deriving Show

instance FromJSON Issue where
  parseJSON (Object v) = do
    -- parents
    let fields    = v .: "fields"
    let priority  = fields >>= (.: "priority")
    -- values
    issueKey     <- v .: "key"
    summary      <- fields >>= (.: "summary")
    description  <- optional $ fields >>= (.: "description")
    priorityName <- optional $ priority >>= (.: "name")
    priorityIcon <- optional $ priority >>= (.: "iconUrl")
    issueStatus  <- optional $ fields >>= (.: "status") >>= (.: "name")
    assigneeName <- optional $ fields >>= (.: "assignee") >>= (.: "displayName")
    creatorName  <- optional $ fields >>= (.: "creator") >>= (.: "displayName")
    commentCount <- optional $ fields >>= (.: "comment") >>= (.: "total")
    let issueUrl  = Nothing
    pure Issue{..}
  parseJSON _ = mempty

respond :: (T.Event, OutputResponse) -> IO ()
respond (evt, resp) = do
  case (trigger $ command evt) of
    "jira" -> fetchIssues (evt, resp)
    "help" -> help (evt, resp)
    _      -> pure ()

help :: (T.Event, OutputResponse) -> IO ()
help (evt, resp) = case (args $ command evt) of
  []       -> say
  ["jira"] -> say
  _        -> pure ()
  where say = slackWriter resp $ SimpleMessage "`!jira AB-123 [AB-234 AB-345…] :: Displays Jira entries`"

fetchIssues :: (T.Event, OutputResponse) -> IO ()
fetchIssues (evt, resp) = mapM_ (fetchIssue resp) (args $ command evt)

fetchIssue :: OutputResponse -> Text -> IO ()
fetchIssue resp issueName = do
  domain <- pack <$> envDefault "jira.atlassian.net" "JIRA_DOMAIN"
  let uri = Data.Text.concat ["https://", domain, "/rest/api/2/issue/", issueName]
  username <- fmap pack <$> envMaybe "JIRA_USERNAME"
  password <- fmap pack <$> envMaybe "JIRA_PASSWORD"
  response <- safeGetUrl uri username password

  -- TODO thread those guys
  case response of
    Left err -> print err
    Right success -> do
      -- print $ toStrict $ decodeUtf8 $ success ^. responseBody
      let status = success ^. responseStatus
      let body = decode (success ^. responseBody) :: Maybe Issue
      case body of
        Just issue -> slackWriter resp $ handle issueName (withUrl domain issue) status
        Nothing    -> pure ()

withUrl :: Text -> Issue -> Issue
withUrl domain issue = issue { issueUrl = Just $ Data.Text.concat ["https://", domain, "/browse/", issueKey issue]}

handle :: Text -> Issue -> Status -> OutputMessage
handle issueName issue status
    | statusIsServerError status = showError issueName
    | statusIsClientError status = showNotFound issueName
    | otherwise = showIssue issue

makeField :: Text -> Maybe Text -> Maybe Field
makeField label val = case val of
  Just v  -> Just $ Field (Just label) v True
  Nothing -> Nothing

showIssue :: Issue -> OutputMessage
showIssue issue = RichMessage defaultAttachment
    { attachmentFallback = summary issue
    , attachmentColor = color
    , attachmentAuthorName = Just (issueKey issue)
    , attachmentAuthorIcon = priorityIcon issue
    , attachmentTitle = Just (summary issue)
    , attachmentTitleLink = issueUrl issue
    , attachmentText = description issue
    , attachmentFields = catMaybes [ makeField "Status" (issueStatus issue)
                                   , makeField "Assigned to" (assigneeName issue)
                                   ]
    , attachmentThumbUrl = Just "https://a.slack-edge.com/2fac/plugins/jira/assets/service_36.png"
    , attachmentFooter = Just $ Data.Text.concat ["Comments: ", pack . show $ (fromMaybe 0 $ commentCount issue)]
    }
  where
    color = case (priorityName issue) of
      Just "Blocker"  -> DangerColor
      Just "Critical" -> DangerColor
      Just "Major"    -> WarningColor
      Just "Minor"    -> GoodColor
      Just "Trivial"  -> GoodColor
      _               -> DefaultColor

showError :: Text -> OutputMessage
showError issueName = SimpleMessage $ Data.Text.concat ["Error fetching item `", issueName, "` from Jira."]

showNotFound :: Text -> OutputMessage
showNotFound issueName = SimpleMessage $ Data.Text.concat ["Item `", issueName, "` not found in Jira."]
