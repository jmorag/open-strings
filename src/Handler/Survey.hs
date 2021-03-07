{-# LANGUAGE TemplateHaskell #-}

module Handler.Survey where

import Import
import Database.Esqueleto (fromSqlKey, toSqlKey)

getSurveyR :: Handler Html
getSurveyR = do
  csrf <- fromMaybe "" . reqToken <$> getRequest
  user_id <- maybeAuthId
  defaultLayout do
    setTitle "Survey"
    $(widgetFile "survey-consent")

getSurveyDemographicsR :: Handler Html
getSurveyDemographicsR = do
  csrf <- fromMaybe "" . reqToken <$> getRequest
  user_id <- maybeAuthId
  -- This is so that if go to the add-work page, we get redirected
  -- back here afterwards. TODO: consider making add-work a modal so
  -- this becomes unnecessary
  setUltDestCurrent
  defaultLayout do
    addAutocomplete
    setTitle "Demographics"
    $(widgetFile "survey-demographics")

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

-- TODO: reduce duplication between here and upload
mkTitle :: Composer -> Work -> Text
mkTitle composer work =
  takeWhile (/= ',') (composerName composer) <> ": "
    <> replaceUnderscores (workTitle work)
