{-# LANGUAGE TemplateHaskell #-}

module Handler.Upload where

import Control.Lens (lengthOf)
import Data.Aeson.Types
import Database.Esqueleto (fromSqlKey, toSqlKey)
import Fingering (Weights, high, low, medium)
import Handler.Pieces
import Import
import Model.Parts
import MusicXML
import Text.Julius
import Text.XML
import Text.XML.Lens
import qualified Data.Set as S

data InferParams = InferParams
  { infer_xml :: !LText
  , infer_weights :: !(Map Text Double)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

postInferR :: Handler Value
postInferR =
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success InferParams {..} -> case parseText def infer_xml of
      Left e -> pure $ object ["error" .= tshow e]
      Right musicxml -> do
        let musicxml' = inferFingerings musicxml infer_weights
        $logInfo "Inferring fingerings"
        result <-
          timeout timeLimit (tryAny (evaluateDeep musicxml'))
        pure case result of
          Nothing -> object ["error" .= timeoutMsg]
          Just (Right xml') ->
            object
              [ "success" .= True
              , "xml" .= renderText def xml'
              ]
          Just (Left e) -> object ["error" .= tshow e]
  where
    timeLimit = 15 * (10 ^ (6 :: Int)) -- 15 seconds
    timeoutMsg :: Text
    timeoutMsg = "Inference timed out. Try annotating some fingerings yourself or uploading a shorter passage"

data UploadFingeringParams = UploadFingeringParams
  { movement_id :: !Int64
  , part :: !Part
  , start_measure :: !Int
  , xml :: !LText
  , description :: !Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

postUploadR :: Handler Value
postUploadR =
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success UploadFingeringParams {..} -> case parseText def xml of
      Left e -> pure $ object ["error" .= tshow e]
      Right musicxml -> do
        let musicxml' = adjustMeasures start_measure musicxml
            end_measure = start_measure + lengthOf (root . measureNumbers) musicxml - 1
        user_id <- requireAuthId
        now <- liftIO getCurrentTime
        runDB . insert_ $
          Entry
            start_measure
            end_measure
            part
            (renderText def musicxml')
            user_id
            (toSqlKey movement_id)
            now
            description
        pure $ object ["success" .= True]

getAddWorkR :: Handler Html
getAddWorkR = defaultLayout do
  csrf <- fromMaybe "" . reqToken <$> getRequest
  addAutocomplete
  addWorkId <- newIdent
  $(widgetFile "add-work")

data AddWorkParams = AddWorkParams
  { work_url :: !(Maybe Text)
  , work_title :: !Text
  , work_composer :: !Text
  , work_instrumentation :: !(Set Part)
  , work_movements :: ![Text]
  , composer_url :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON AddWorkParams

postAddWorkR :: Handler Value
postAddWorkR =
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success AddWorkParams {..} -> runDB do
      composerId <-
        insertBy $ Composer (addUnderscores work_composer) (addUnderscores <$> composer_url)
      work <-
        insertBy $
          Work
            (addUnderscores work_title)
            work_url
            work_instrumentation
            (either entityKey id composerId)
      case work of
        Left (Entity workId _) ->
          pure $
            object
              [ "error" .= ("Work already in database" :: Text)
              , "work_id" .= fromSqlKey workId
              ]
        Right workId -> do
          case work_movements of
            [] -> insert_ $ Movement 0 "" workId
            [m] -> insert_ $ Movement 0 m workId
            ms -> insertMany_ $ zipWith (\i m -> Movement i m workId) [1 ..] ms
          pure $ object ["work_id" .= fromSqlKey workId]

getWorkR :: Int64 -> Handler Html
getWorkR work_key = do
  let workId = toSqlKey work_key
      jsWorkId = toJSON work_key
  work <- runDB $ get404 workId
  composer <- runDB $ get404 (workComposerId work)
  entries <- getEntriesR work_key
  csrf <- fromMaybe "" . reqToken <$> getRequest
  mentryId <- lookupGetParam "entry-id"
  let entryId = fromMaybe Null (fmap Number . readMay =<< mentryId)
      title = mkTitle composer work
  (parts, movements) <- workData work_key
  user_id <- maybeAuthId
  defaultLayout do
    setTitle (toHtml title)
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScript (StaticR js_fingeringeditor_js)
    wId <- newIdent
    renderId <- newIdent
    $(widgetFile "work")

mkTitle :: Composer -> Work -> Text
mkTitle composer work =
  takeWhile (/= ',') (composerName composer) <> ": "
    <> replaceUnderscores (workTitle work)

getEntryR :: Int64 -> Handler Html
getEntryR entry_key = do
  let entryId = toSqlKey entry_key
  (entry, movement, work, composer, uploadedBy) <- runDB do
    e <- get404 entryId
    m <- get404 (entryMovementId e)
    w <- get404 (movementWorkId m)
    c <- get404 (workComposerId w)
    u <- get404 (entryUploadedBy e)
    pure (e, m, w, c, u)
  let title = mkTitle composer work
      time = toJSON (entryCreatedAt entry)
  uploadedByName <- formatUsername (Entity (entryUploadedBy entry) uploadedBy)
  csrf <- fromMaybe "" . reqToken <$> getRequest
  defaultLayout do
    renderId <- newIdent
    setTitle (toHtml title)
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScript (StaticR js_fingeringeditor_js)
    $(widgetFile "entry")

startingWeights :: Value
startingWeights =
  object
    [ "same string" .= (- high)
    , "same position" .= (- high)
    , "open string" .= zero
    , "fourth finger" .= zero
    , "high position" .= zero
    , "medium position" .= zero
    ]
  where
    zero = 0 :: Double
