{-# LANGUAGE TemplateHaskell #-}

module Handler.Survey where

import Import

getSurveyR :: Handler Html
getSurveyR = defaultLayout do
  setTitle "Survey"
  $(widgetFile "survey")
