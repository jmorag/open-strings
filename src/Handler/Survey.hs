{-# LANGUAGE TemplateHaskell #-}

module Handler.Survey where

import Import
import Database.Esqueleto (fromSqlKey, toSqlKey)
import Data.Aeson.Types

getSurveyR :: Handler Html
getSurveyR = do
  csrf <- fromMaybe "" . reqToken <$> getRequest
  user_id <- maybeAuthId
  defaultLayout do
    setTitle "Survey"
    $(widgetFile "survey-consent")

postSurveyR :: Handler Html
postSurveyR = do
  user_id <- requireAuthId
  runDB $ update user_id [UserSurveyAgree =. True]
  redirect SurveyDemographicsR

getSurveyDemographicsR :: Handler Html
getSurveyDemographicsR = do
  csrf <- fromMaybe "" . reqToken <$> getRequest
  user_id <- requireAuthId
  -- This is so that if go to the add-work page, we get redirected
  -- back here afterwards. TODO: consider making add-work a modal so
  -- this becomes unnecessary
  setUltDestCurrent
  defaultLayout do
    addAutocomplete
    setTitle "Demographics"
    $(widgetFile "survey-demographics")

postSurveyDemographicsR :: Handler Value
postSurveyDemographicsR = do
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success (demographics :: SurveyDemographics) -> do
      runDB $ insert_ demographics
      renderUrl <- getUrlRender
      pure $ object ["destination" .= renderUrl (SurveyFingeringR 1)] -- TODO: real destination WIP

getSurveyFingeringR :: Int64 -> Handler Html
getSurveyFingeringR entry_key = do
  let entryId = toSqlKey entry_key
  -- TODO: use a real join
  (entry, movement, work, composer) <- runDB do
    e <- get404 entryId
    m <- get404 (entryMovementId e)
    w <- get404 (movementWorkId m)
    c <- get404 (workComposerId w)
    pure (e, m, w, c)
  let title = mkTitle composer work
  csrf <- fromMaybe "" . reqToken <$> getRequest
  user_id <- maybeAuthId
  defaultLayout do
    setTitle "Question"
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScript (StaticR js_fingeringeditor_js)
    $(widgetFile "survey-question")
