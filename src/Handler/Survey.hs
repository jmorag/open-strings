{-# LANGUAGE TemplateHaskell #-}

module Handler.Survey (
  getSurveyR,
  postSurveyR,
  getSurveyDemographicsR,
  postSurveyDemographicsR,
  getSurveyFingeringR,
  postSurveyFingeringR,
  getSurveyDoneR,
) where

import Data.Aeson.Types
import Data.List (findIndex)
import Database.Esqueleto (fromSqlKey, toSqlKey)
import Import hiding (index)

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
  Entity user_id user <- requireAuth
  unless (userSurveyAgree user) $ redirect SurveyR
  -- This is so that if go to the add-work page, we get redirected
  -- back here afterwards. TODO: make add-work a modal so this becomes
  -- unnecessary
  setUltDestCurrent
  defaultLayout do
    addAutocomplete
    setTitle "Demographics"
    $(widgetFile "survey-demographics")

getSurveyEntries :: Handler [Entity Entry]
getSurveyEntries = runDB $ selectList [EntryIsAlgorithm !=. Nothing] []

postSurveyDemographicsR :: Handler Value
postSurveyDemographicsR =
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success (demographics@SurveyDemographics {..}) -> do
      runDB do
        upsertBy
          (UniqueUserId surveyDemographicsUserId)
          demographics
          [ SurveyDemographicsYears_experience
              =. surveyDemographicsYears_experience
          , SurveyDemographicsViolinist_type =. surveyDemographicsViolinist_type
          , SurveyDemographicsMost_recent_piece =. surveyDemographicsMost_recent_piece
          ]
      getSurveyEntries >>= \case
        [] -> pure $ object ["error" .= ("No survey entries found" :: Text)]
        (Entity exampleId _ : _) ->
          destination (SurveyFingeringR (fromSqlKey exampleId))

getSurveyFingeringR :: Int64 -> Handler Html
getSurveyFingeringR entry_key = do
  Entity user_id user <- requireAuth
  unless (userSurveyAgree user) $ redirect SurveyR
  runDB (getBy (UniqueUserId user_id)) >>= \case
    Nothing -> redirect SurveyDemographicsR
    Just (Entity surveyId _) -> do
      surveyEntries <- getSurveyEntries
      let entryId = toSqlKey entry_key
      case findIndex (\(Entity eId _) -> entryId == eId) surveyEntries of
        Nothing -> notFound
        Just index -> do
          (entry, movement, work, composer) <- runDB do
            e <- get404 entryId
            m <- get404 (entryMovementId e)
            w <- get404 (movementWorkId m)
            c <- get404 (workComposerId w)
            pure (e, m, w, c)
          let title = mkTitle composer work
          csrf <- fromMaybe "" . reqToken <$> getRequest
          defaultLayout do
            setTitle "Question"
            addScript (StaticR js_opensheetmusicdisplay_min_js)
            addScript (StaticR js_fingeringeditor_js)
            $(widgetFile "survey-question")

postSurveyFingeringR :: Int64 -> Handler Value
postSurveyFingeringR entry_key =
  parseCheckJsonBody >>= \case
    Error e -> pure $ object ["error" .= e]
    Success (response@SurveyFingering {..}) -> do
      runDB do
        upsertBy
          (UniqueSurveyResponse surveyFingeringEntryId surveyFingeringSurveyId)
          response
          [ SurveyFingeringComfort =. surveyFingeringComfort
          , SurveyFingeringExpressivity =. surveyFingeringExpressivity
          , SurveyFingeringIdiomatic =. surveyFingeringIdiomatic
          , SurveyFingeringFingering_guess =. surveyFingeringFingering_guess
          , SurveyFingeringOther =. surveyFingeringOther
          ]
      let entryId = toSqlKey entry_key
      surveyEntries <- getSurveyEntries
      case findNext (\(Entity eId _) -> entryId == eId) surveyEntries of
        Nothing -> destination SurveyDoneR
        Just (Entity nextId _) ->
          destination (SurveyFingeringR (fromSqlKey nextId))

getSurveyDoneR :: Handler Html
getSurveyDoneR = defaultLayout do
  setTitle "Survey Complete"
  [whamlet|
<h1>Survey complete
  <p .lead>Thank you for taking the OpenStrings survey!
|]

destination :: Route App -> Handler Value
destination route = do
  renderUrl <- getUrlRender
  pure $ object ["destination" .= renderUrl route]

findNext :: (a -> Bool) -> [a] -> Maybe a
findNext f (x : y : _) | f x = Just y
findNext f (_ : xs) = findNext f xs
findNext _ [] = Nothing
