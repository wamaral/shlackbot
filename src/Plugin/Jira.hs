{-# LANGUAGE RecordWildCards #-}

module Plugin.Jira
  ( respond
  , hear
  , help
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.List            (nub)
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
-- import           Data.Text.Lazy          (toStrict)
-- import           Data.Text.Lazy.Encoding
import           Network.HTTP.Types
import           Network.Wreq
import           Text.Megaparsec      hiding (label)
import           Text.Megaparsec.Text
import           Types
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

data IssueResponseType = Short | Full

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

respond :: BotResponse
respond (evt, resp) = do
  case command evt of
    Just cmd -> case trigger cmd of
      "jira" -> fetchIssues (args cmd) resp
      "help" -> help (args cmd) resp
      _      -> pure ()
    Nothing -> pure ()

hear :: BotResponse
hear (evt, resp) = do
  fetchShortIssues (T.words $ fullMessage evt) resp

help :: [Text] -> OutputResponse -> IO ()
help args resp = case args of
  []       -> say
  ["jira"] -> say
  _        -> pure ()
  where say = slackWriter resp $ SimpleMessage "`!jira XYZ-123 [XYZ-234 XYZ-345…] :: Displays Jira entries`"

isIssueKey :: Text -> Bool
isIssueKey = isJust . parseMaybe issueKeyParser
  where
    issueKeyParser :: Parser String
    issueKeyParser = some letterChar >> char '-' >> some numberChar

getIssues :: [Text] -> [Text]
getIssues = nub . map T.toUpper . filter isIssueKey

fetchShortIssues :: [Text] -> OutputResponse -> IO ()
fetchShortIssues message resp = mapM_ (fetchIssue Short resp) $ getIssues message

fetchIssues :: [Text] -> OutputResponse -> IO ()
fetchIssues args resp = mapM_ (fetchIssue Full resp) $ getIssues args

fetchIssue :: IssueResponseType -> OutputResponse -> Text -> IO ()
fetchIssue respType resp issueName = do
  domain <- T.pack <$> envDefault "jira.atlassian.net" "JIRA_DOMAIN"
  let uri = T.concat ["https://", domain, "/rest/api/2/issue/", issueName]
  username <- fmap T.pack <$> envMaybe "JIRA_USERNAME"
  password <- fmap T.pack <$> envMaybe "JIRA_PASSWORD"
  response <- safeGetUrl uri username password

  -- TODO thread those guys
  case response of
    Left err -> print $ T.concat ["[JIRA] Error: ", err, " :: issue ", issueName]
    Right success -> do
      print $ T.concat ["[JIRA] Fetched issue ", issueName]
      -- print $ toStrict $ decodeUtf8 $ success ^. responseBody
      let status = success ^. responseStatus
      let body = decode (success ^. responseBody) :: Maybe Issue
      case body of
        Just issue -> slackWriter resp $ handle respType issueName (withUrl domain issue) status
        Nothing    -> pure ()

withUrl :: Text -> Issue -> Issue
withUrl domain issue = issue { issueUrl = Just $ T.concat ["https://", domain, "/browse/", issueKey issue]}

handle :: IssueResponseType -> Text -> Issue -> Status -> OutputMessage
handle respType issueName issue status
    | statusIsServerError status = showError issueName
    | statusIsClientError status = showNotFound issueName
    | otherwise = case respType of
        Full  -> showIssue issue
        Short -> showShortIssue issue

makeField :: Text -> Maybe Text -> Maybe Field
makeField label val = case val of
  Just v  -> Just $ Field (Just label) v True
  Nothing -> Nothing

showShortIssue :: Issue -> OutputMessage
showShortIssue issue = QuotedSimpleMessage $ T.intercalate " :: " $ nub $ sequence [issueKey, summary, (fromMaybe "") . issueStatus] issue

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
    , attachmentFooter = Just $ T.concat ["Comments: ", T.pack . show $ (fromMaybe 0 $ commentCount issue)]
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
showError issueName = SimpleMessage $ T.concat ["Error fetching item `", issueName, "` from Jira."]

showNotFound :: Text -> OutputMessage
showNotFound issueName = SimpleMessage $ T.concat ["Item `", issueName, "` not found in Jira."]
