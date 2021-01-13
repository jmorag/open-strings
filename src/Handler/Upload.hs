{-# LANGUAGE TemplateHaskell #-}

module Handler.Upload where

import Control.Lens (deep, lengthOf, (^..))
import Data.Aeson.Types
import Database.Esqueleto (fromSqlKey, toSqlKey)
import Handler.Pieces
import Import
import Model.Parts
import MusicXML
import Text.Julius
import Text.XML
import Text.XML.Lens

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
        let xml' = inferFingerings musicxml infer_weights
        print (xml' ^.. root . deep (el "string") . text)
        pure $
          object
            [ "success" .= True
            , "xml" .= renderText def xml'
            ]

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
      title =
        takeWhile (/= ',') (composerName composer) <> ": "
          <> replaceUnderscores (workTitle work)
  (parts, movements) <- workData work_key
  user_id <- maybeAuthId
  defaultLayout do
    setTitle (toHtml title)
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScript (StaticR js_fingeringeditor_js)
    wId <- newIdent
    renderId <- newIdent
    $(widgetFile "work")
