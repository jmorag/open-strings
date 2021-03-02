{-# LANGUAGE TemplateHaskell #-}

module Handler.Survey where

import Import

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
  defaultLayout do
    addAutocomplete
    setTitle "Demographics"
    $(widgetFile "survey-demographics")
