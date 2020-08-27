{-# LANGUAGE TemplateHaskell #-}

module Handler.Upload where

import Control.Lens hiding ((.=))
import Data.Aeson.Types
import Data.Char
import qualified Data.Text as T
import Database.Esqueleto hiding (Value)
import qualified Database.Esqueleto as E
import Import
import Model.Parts
import MusicXML
import Text.Julius
import Text.XML
import Text.XML.Lens

data UploadFingeringParams = UploadFingeringParams
  { movement_id :: !Int64,
    part :: !Part,
    start_measure :: !Int,
    xml :: !LText,
    description :: !Text
  }
  deriving (Show, Generic)

instance FromJSON UploadFingeringParams

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

getUploadR :: Handler Html
getUploadR = do
  user_id <- maybeAuthId
  defaultLayout do
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScript (StaticR js_fingeringeditor_js)
    addAutocomplete
    uploadId <- newIdent
    renderId <- newIdent
    $(widgetFile "upload-fingering")

addAutocomplete :: WidgetFor App ()
addAutocomplete = do
  addScript (StaticR js_autocomplete_js)
  addStylesheet (StaticR css_autocomplete_css)

getAddWorkR :: Handler Html
getAddWorkR = defaultLayout do
  addAutocomplete
  addWorkId <- newIdent
  $(widgetFile "add-work")

data AddWorkParams = AddWorkParams
  { work_url :: !(Maybe Text),
    work_title :: !Text,
    work_composer :: !Text,
    work_instrumentation :: !(Set Part),
    work_movements :: ![Text],
    composer_url :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON AddWorkParams

addUnderscores :: Text -> Text
addUnderscores = T.map \c -> if isSpace c then '_' else c

postAddWorkR :: Handler Value
postAddWorkR =
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success AddWorkParams {..} -> runDB do
      composerId <-
        insertBy $ Composer (addUnderscores work_composer) (addUnderscores <$> composer_url)
      work <-
        insertUnique $
          Work
            (addUnderscores work_title)
            work_url
            work_instrumentation
            (either entityKey id composerId)
      case work of
        Nothing -> pure $ object ["error" .= ("Work already in database" :: Text)]
        Just workId -> do
          case work_movements of
            [] -> insert_ $ Movement 0 "" workId
            [m] -> insert_ $ Movement 0 m workId
            ms -> insertMany_ $ zipWith (\i m -> Movement i m workId) [1 ..] ms
          pure $ object ["success" .= True]
