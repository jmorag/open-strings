import Import.NoFoundation

import Control.Monad.Logger (runStderrLoggingT)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.URL
import Model

import System.Exit (die)
import System.Process (callProcess)

main =
  getArgs >>= \case
    [dbUrl] -> run dbUrl
    _ -> die "Usage: survey-plot [database-url]"

run dbUrl = do
  PostgresConf {..} <- fromDatabaseUrl 1 dbUrl
  runStderrLoggingT $
    withPostgresqlPool pgConnStr pgPoolSize $
      \pool -> liftIO $
        flip runSqlPersistMPool pool $ do
          responses <- getSurveyResponses
          let csv = encodeDefaultOrderedByName responses
          liftIO do
            BL.writeFile "survey/survey_responses.csv" csv
            callProcess "python" ["survey/plots.py"]

data SurveyResponse = SurveyResponse
  { user :: Maybe Text
  , survey_id :: Int64
  , years_experience :: Int
  , violinist_type :: Text -- concat multiple with commas
  , most_recent_piece :: Text -- human readable
  , survey_question_id :: Int64
  , piece :: Text -- human readable
  , comfort :: Int
  , expressivity :: Int
  , idiomatic :: Int
  , fingering_guess :: Int
  , other :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToNamedRecord, DefaultOrdered)

getSurveyResponses :: (MonadUnliftIO m) => ReaderT SqlBackend m [SurveyResponse]
getSurveyResponses = do
  keys <- selectKeysList [] []
  traverse getSurveyResponse keys

getSurveyResponse ::
  (MonadUnliftIO m) => Key SurveyFingering -> ReaderT SqlBackend m SurveyResponse
getSurveyResponse survey_fingering_id = do
  SurveyFingering {..} <- getJust survey_fingering_id
  SurveyDemographics {..} <- getJust surveyFingeringSurveyId
  Entity _ (OAuthUser {..}) <-
    getBy404 $
      UniqueOAuthUserId surveyDemographicsUserId
  most_recent_piece <- formatWork surveyDemographicsMost_recent_piece
  piece <- formatEntry surveyFingeringEntryId
  pure
    SurveyResponse
      { user = oAuthUserName
      , survey_id = fromSqlKey surveyFingeringSurveyId
      , years_experience = surveyDemographicsYears_experience
      , violinist_type =
          T.intercalate
            ","
            (foldMap (\t -> [tshow t]) surveyDemographicsViolinist_type)
      , most_recent_piece
      , survey_question_id = fromSqlKey survey_fingering_id
      , piece
      , comfort = surveyFingeringComfort
      , expressivity = surveyFingeringExpressivity
      , idiomatic = surveyFingeringIdiomatic
      , fingering_guess = surveyFingeringFingering_guess
      , other = surveyFingeringOther
      }

formatWork ::
  (MonadUnliftIO m) => Key Work -> ReaderT SqlBackend m Text
formatWork work_id = do
  Work {..} <- getJust work_id
  Composer {..} <- getJust workComposerId
  pure $ composerName <> ": " <> workTitle

formatEntry entry_id = do
  Entry {..} <- getJust entry_id
  Movement {..} <- getJust entryMovementId
  title <- formatWork movementWorkId
  let movement = case movementNumber of
        0 -> ""
        n -> tshow n <> ". " <> movementName
      measures = tshow entryMeasure_start <> "-" <> tshow entryMeasure_end
  pure $ title <> " " <> movement <> " mm " <> measures
