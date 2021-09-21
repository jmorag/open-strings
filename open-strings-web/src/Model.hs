{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import ClassyPrelude.Yesod
import Data.Char
import Data.FileEmbed (makeRelativeToProject)
import qualified Data.Text as T
import Database.Persist.Quasi
import Model.Parts
import Model.Survey
import Model.UserType

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings =<< makeRelativeToProject "config/models.persistentmodels")

mkTitle :: Composer -> Work -> Text
mkTitle composer work =
  takeWhile (/= ',') (composerName composer) <> ": "
    <> replaceUnderscores (workTitle work)

addUnderscores :: Text -> Text
addUnderscores = T.map \c -> if isSpace c then '_' else c

replaceUnderscores :: Text -> Text
replaceUnderscores = T.map \case '_' -> ' '; c -> c
