import Control.Monad.Logger (runStderrLoggingT)
import qualified Data.Set as Set
import Database.Persist
import Database.Persist.Postgresql
import Dhall
import Import
import Model.Parts (Part)

data DhallComposer = DhallComposer
  { name :: Text,
    url :: Maybe Text,
    works :: [DhallWork]
  }
  deriving (Show, Generic)

data DhallWork = DhallWork
  { title :: Text,
    url :: Maybe Text,
    instrumentation :: [Part],
    movements :: [Text]
  }
  deriving (Show, Generic)

instance FromDhall DhallComposer

instance FromDhall DhallWork

insertOrGet x =
  insertBy x <&> \case
    Left entity -> entityKey entity
    Right key -> key

insertOrGet_ x = Import.void (insertOrGet x)

insertComposer :: (MonadUnliftIO m) => DhallComposer -> ReaderT SqlBackend m ()
insertComposer DhallComposer {..} = do
  composerId <- insertOrGet (Composer name url)
  forM_ works \DhallWork {..} -> do
    workId <-
      insertOrGet $ Work title url (Set.fromList instrumentation) composerId
    case movements of
      [] -> insertOrGet_ $ Movement 0 "" workId
      [m] -> insertOrGet_ $ Movement 0 m workId
      ms -> forM_ (zip [1 ..] ms) \(i, m) -> insertOrGet_ (Movement i m workId)

main = do
  runStderrLoggingT $
    withPostgresqlPool "dbname=fingerdb" 10 $
      \pool -> liftIO $
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll
          composers <- liftIO $ inputFile auto "dhall-to-db/pieces/pieces.dhall"
          traverse_ @[_] insertComposer composers
