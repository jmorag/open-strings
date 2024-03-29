{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Upload where

import Control.Lens (lengthOf)
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Database.Esqueleto (fromSqlKey, toSqlKey)
import Fingering (Weights, high, low, medium)
import Handler.Pieces
import Import
import Model.Parts
import MusicXML
import Text.Julius
import Text.XML
import Text.XML.Lens
import Visualization
import Yesod.Core (invalidArgs)

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
        $logInfo "Inferring fingerings"
        let musicxml' = inferFingerings musicxml infer_weights
        result <- timeout 30e6 (tryAny (evaluateDeep musicxml'))
        pure case result of
          Nothing -> object ["error" .= timeoutMsg]
          Just (Right (cost, xml')) ->
            object
              [ "success" .= True
              , "xml" .= renderText def xml'
              , "cost" .= cost
              ]
          Just (Left e) -> object ["error" .= tshow e]
  where
    timeoutMsg :: Text
    timeoutMsg = "Inference timed out. Try annotating some fingerings yourself or selected a smaller section."

postInferWeightsR :: Handler Value
postInferWeightsR =
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success InferParams {..} -> case parseText def infer_xml of
      Left e -> pure $ object ["error" .= tshow e]
      Right musicxml -> do
        $logInfo "Inferring weights"
        let weights' = inferWeights musicxml (M.fromList startingWeights)
        result <- tryAny (evaluateDeep weights')
        pure . object $ case result of
          Left e -> ["error" .= tshow e]
          Right eWeights -> case eWeights of
            Left e -> ["error" .= e]
            Right weights ->
              let toJSWeight (k, _) =
                    object ["name" .= k, "value" .= (round (weights M.! k) :: Int)]
               in [ "success" .= True
                  , "infer_weights" .= array (map toJSWeight startingWeights)
                  ]

postVisualizeR :: Handler Html
postVisualizeR = do
  params@VisualizeParams {..} <- requireCheckJsonBody
  case parseText def infer_xml of
    Left e -> invalidArgs ["Unable to parse xml", tshow e]
    Right musicxml -> do
      $logInfo "Generating visualization statically"
      pure $ renderGraph musicxml params

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
        entryId <-
          runDB . insert $
            Entry
              start_measure
              end_measure
              part
              (renderText def musicxml')
              user_id
              (toSqlKey movement_id)
              now
              description
              Nothing
        pure $ object ["success" .= True, "entry_id" .= entryId]

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
postAddWorkR = do
  user_id <- maybeAuthId
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success AddWorkParams {..} -> runDB do
      composerId <-
        insertBy $
          Composer
            (addUnderscores work_composer)
            (addUnderscores <$> composer_url)
            user_id
      work <-
        insertBy $
          Work
            (addUnderscores work_title)
            work_url
            work_instrumentation
            (either entityKey id composerId)
            user_id
      case work of
        Left (Entity workId _) ->
          pure $
            object
              [ "already_uploaded" .= True
              , "work_id" .= fromSqlKey workId
              ]
        Right workId -> do
          case work_movements of
            [] -> insert_ $ Movement 0 "" workId
            [m] -> insert_ $ Movement 0 m workId
            ms -> insertMany_ $ zipWith (\i m -> Movement i m workId) [1 ..] ms
          pure $
            object
              [ "label" .= (work_composer <> ": " <> work_title)
              , "work_id" .= (fromSqlKey workId)
              ]

getWorkR :: Int64 -> Handler Html
getWorkR work_key = do
  let workId = toSqlKey work_key
      jsWorkId = toJSON work_key
  work <- runDB $ get404 workId
  composer <- runDB $ get404 (workComposerId work)
  entries <- getEntriesR work_key
  csrf <- fromMaybe "" . reqToken <$> getRequest
  let title = mkTitle composer work
  (parts, movements) <- workData work_key
  user_id <- maybeAuthId
  defaultLayout do
    setTitle (toHtml title)
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScript (StaticR js_fingeringeditor_js)
    wId <- newIdent
    renderId <- newIdent
    $(widgetFile "work")

getEntryR :: Int64 -> Handler Html
getEntryR entry_key = do
  let entryId = toSqlKey entry_key
  -- TODO: use a real join
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
  user_id <- maybeAuthId
  defaultLayout do
    renderId <- newIdent
    setTitle (toHtml title)
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScriptRemote "https://cdn.jsdelivr.net/npm/svg-drag-select@0.4.2"
    addScriptRemote "https://d3js.org/d3.v7.min.js"
    addScript (StaticR js_fingeringeditor_js)
    let subtitle = case movementNumber movement of
          0 -> case movementName movement of "" -> Nothing; name -> Just name
          num -> Just $ tshow num <> ". " <> movementName movement
    $(widgetFile "entry")

startingWeights :: [(Text, Double)]
startingWeights =
  [ ("same string", - 97)
  , ("same position", - 87)
  , ("open string", -76)
  , ("fourth finger", 11)
  , ("high position", 73)
  , ("medium position", 34)
  , ("double string crossing", 91)
  , ("oblique finger crossing", 100)
  , ("one finger half step shift", 0)
  , ("augmented second 1-2, 2-3", 41)
  ]

-- Construct this separately so that sliders are displayed in the right order
jsStartingWeights :: Value
jsStartingWeights =
  array $
    map (\(nm, val) -> object ["name" .= nm, "value" .= val]) startingWeights
