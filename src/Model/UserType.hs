module Model.UserType where

import ClassyPrelude.Yesod
import Database.Persist.Sql

data UserType = Email | OAuth
  deriving (Generic, Show, Eq)

instance PersistField UserType where
  toPersistValue s = case s of
    Email -> PersistBool True
    OAuth -> PersistBool False
  fromPersistValue (PersistBool b) = if b then Right Email else Right OAuth
  fromPersistValue x = Left $ "File.hs: When trying to deserialize a UserType: expected PersistBool, received: " <> tshow x

instance PersistFieldSql UserType where
  sqlType _ = SqlBool
