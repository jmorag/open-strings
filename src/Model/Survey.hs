{-# LANGUAGE TemplateHaskell #-}

module Model.Survey where

import ClassyPrelude.Yesod

data ViolinistType
  = Amateur
  | StudentBeginner
  | StudentIntermediate
  | StudentAdvanced
  | ProfessionalSolo
  | ProfessionalChamber
  | ProfessionalOrchestra
  deriving (Generic, Show, Eq, Read, Ord)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "ViolinistType"

data FingeringTypeGuess
  = DefinitelyAlgorithm
  | MaybeAlgorithm
  | Unsure
  | MaybeHuman
  | DefinitelyHuman
  deriving (Generic, Show, Eq, Read)
  deriving anyclass (FromJSON, ToJSON)
derivePersistField "FingeringTypeGuess"
