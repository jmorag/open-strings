module Handler.Pieces where

import Control.Lens ((&))
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.Persist.Sql
import Import

getComposersR :: Handler Value
getComposersR = do
  term <- fromMaybe "" <$> lookupGetParam "term"
  composers <- runDB $ mkQuery (words term)
  composers & map String & V.fromList & Array & pure

mkQuery :: (MonadUnliftIO m) => [Text] -> ReaderT SqlBackend m [Text]
mkQuery [] = pure []
mkQuery names =
  map unSingle
    <$> rawSql
      ( "select full_name from composers where "
          <> T.intercalate " AND " (map (\_ -> "full_name ILIKE ?") names)
      )
      (map (PersistText . fuzzy) names)

fuzzy :: Text -> Text
fuzzy s = "%" <> s <> "%"

getWorksR :: Handler Value
getWorksR = do
  composer <- fromMaybe "" <$> lookupGetParam "composer"
  composerKey <- runDB $ getBy (UniqueComposerName composer)
  case composerKey of
    Nothing -> pure (Array (V.fromList []))
    Just cKey -> do
      works <- runDB $ selectList [WorkComposerId ==. entityKey cKey] []
      pure $
        works
          & map (String . T.dropEnd (T.length composer + 3). workWork_id . entityVal)
          & V.fromList
          & Array
