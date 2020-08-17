module Handler.Pieces where

import Data.Aeson.Types (emptyArray)
import qualified Data.Text as T
import Database.Esqueleto hiding (Value)
import qualified Database.Esqueleto as E
import Import hiding ((==.))
import Model.Parts

getComposersR :: Handler Value
getComposersR = do
  query <- fromMaybe "" <$> lookupGetParam "term"
  composers <- runDB do
    select $ from \c -> do
      forM_ (T.words query) \q -> where_ $ c ^. ComposerName `ilike` (fuzzy q)
      pure (c ^. ComposerId, c ^. ComposerName)
  pure . array $ map composerObject composers
  where
    composerObject (E.Value key, E.Value comp) =
      object
        ["label" .= replaceUnderscores comp, "value" .= key]

replaceUnderscores :: Text -> Text
replaceUnderscores = T.map \case '_' -> ' '; c -> c

getWorksR :: Handler Value
getWorksR = do
  query <- fromMaybe "" <$> lookupGetParam "term"
  allWorks <- runDB . select $ from \(composer `InnerJoin` work) -> do
    E.on (work ^. WorkComposerId ==. composer ^. ComposerId)
    forM_ (T.words (T.filter (/= ':') query)) \q ->
      let q' = fuzzy q
       in where_ $
            (composer ^. ComposerName `ilike` q')
              E.||. (work ^. WorkTitle `ilike` q')
    orderBy [asc (composer ^. ComposerName), asc (work ^. WorkTitle)]
    pure (composer ^. ComposerName, work ^. WorkId, work ^. WorkTitle)
  pure $ array (map formatWork allWorks)

formatWork :: (E.Value Text, E.Value WorkId, E.Value Text) -> Value
formatWork (E.Value composer, E.Value workKey, E.Value title) =
  object
    [ "label" .= replaceUnderscores (composer <> ": " <> title),
      "value" .= workKey
    ]

fuzzy :: Text -> SqlExpr (E.Value Text)
fuzzy t = (%) ++. val t ++. (%)

getMovementsR :: Int64 -> Handler Value
getMovementsR workId = do
  (work :: Maybe Work, movements) <-
    runDB $
      liftA2
        (,)
        ( get (fromBackendKey (SqlBackendKey workId))
        )
        ( select $
            from $ \(work `InnerJoin` movement) -> do
              E.on (work ^. WorkId ==. movement ^. MovementWorkId)
              where_ (work ^. WorkId ==. valkey workId)
              orderBy [asc (movement ^. MovementNumber)]
              pure (movement ^. MovementId, movement ^. MovementNumber, movement ^. MovementName)
        )
  pure $
    object
      [ "parts" .= maybe emptyArray (toJSON . workInstrumentation) work,
        "movements" .= array (map jsonMovement movements)
      ]
  where
    jsonMovement (E.Value key, E.Value i, E.Value movement) =
      object ["text" .= (tshow i <> ". " <> movement), "value" .= key]

getEntriesR :: Int64 -> Handler Value
getEntriesR workId = do
  entries <- runDB $
    select $
      from $ \(user `InnerJoin` entry `InnerJoin` movement) -> do
        E.on (entry ^. EntryUploadedBy ==. user ^. UserId)
        E.on (entry ^. EntryMovementId ==. movement ^. MovementId)
        where_ (movement ^. MovementWorkId ==. valkey workId)
        orderBy [asc (entry ^. EntryMeasure_start)]
        pure
          ( movement ^. MovementNumber,
            movement ^. MovementName,
            movement ^. MovementId,
            entry ^. EntryPart,
            entry ^. EntryMeasure_start,
            entry ^. EntryMeasure_end,
            entry ^. EntryDescription,
            user ^. UserName,
            entry ^. EntryCreatedAt,
            entry ^. EntryId
          )
  pure . array $
    map
      ( \( E.Value movementNum,
           E.Value movement,
           E.Value movementId,
           E.Value part,
           E.Value start,
           E.Value end,
           E.Value description,
           E.Value user,
           E.Value time,
           E.Value entryId
           ) ->
            object
              [ "movement" .= (tshow movementNum <> ". " <> movement),
                "part" .= part,
                "measures" .= (tshow start <> " - " <> tshow end),
                "start_measure" .= start,
                "description" .= description,
                "uploaded_by" .= object ["user" .= user, "time" .= time],
                "movement_id" .= movementId,
                "entry_id" .= entryId
              ]
      )
      entries

getMusicXMLR :: Int64 -> Handler LText
getMusicXMLR entryId = entryMusicxml <$> runDB (get404 (toSqlKey entryId))
