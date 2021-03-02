module Model.UserType where

import ClassyPrelude.Yesod
import Database.Persist.Sql

-- I learned about derivePersistField after writing this and pushing
-- it to production. Future custom model types should not manually
-- implement serialization.
data UserType = Email | OAuth
  deriving (Generic, Show, Eq)

instance PersistField UserType where
  toPersistValue = \case
    Email -> PersistBool True
    OAuth -> PersistBool False
  fromPersistValue (PersistBool b) = if b then Right Email else Right OAuth
  fromPersistValue x = Left $ "UserType.hs: When trying to deserialize a UserType: expected PersistBool, received: " <> tshow x

instance PersistFieldSql UserType where
  sqlType _ = SqlBool
