module Handler.Pieces where

import Data.Aeson.Types (emptyArray)
import qualified Data.Text as T
import Database.Esqueleto hiding (Value)
import qualified Database.Esqueleto as E
import Database.Esqueleto.Internal.Sql (unsafeSqlFunction)
import Import hiding ((==.))
import Model.UserType

getComposersR :: Handler Value
getComposersR = do
  query <- fromMaybe "" <$> lookupGetParam "term"
  composers <- runDB do
    select $ from \c -> do
      forM_ (T.words query) \q -> where_ $ unaccent (c ^. ComposerName) `ilike` (fuzzy q)
      pure (c ^. ComposerId, c ^. ComposerName)
  pure . array $ map composerObject composers
  where
    composerObject (E.Value key, E.Value comp) =
      object
        ["label" .= replaceUnderscores comp, "value" .= key]

getWorksR :: Handler Value
getWorksR = do
  query <- fromMaybe "" <$> lookupGetParam "term"
  allWorks <- runDB . select $ from \(composer `InnerJoin` work) -> do
    E.on (work ^. WorkComposerId ==. composer ^. ComposerId)
    forM_ (T.words (T.filter (/= ':') query)) \q ->
      let q' = fuzzy q
       in where_ $
            (unaccent (composer ^. ComposerName) `ilike` q')
              E.||. (unaccent (work ^. WorkTitle) `ilike` q')
    orderBy [asc (composer ^. ComposerName), asc (work ^. WorkTitle)]
    pure (composer ^. ComposerName, work ^. WorkId, work ^. WorkTitle)
  pure $ array (map formatWork allWorks)

formatWork :: (E.Value Text, E.Value WorkId, E.Value Text) -> Value
formatWork (E.Value composer, E.Value workKey, E.Value title) =
  object
    [ "label" .= replaceUnderscores (composer <> ": " <> title)
    , "value" .= workKey
    ]

fuzzy :: Text -> SqlExpr (E.Value Text)
fuzzy t = (%) ++. val t ++. (%)

-- See https://stackoverflow.com/questions/3051762/comparing-strings-in-postgresql
unaccent :: SqlString s => SqlExpr (E.Value s) -> SqlExpr (E.Value s)
unaccent = unsafeSqlFunction "unaccent_string"

workData :: Int64 -> Handler (Value, Value)
workData workId = do
  (work, movements) <-
    runDB $
      liftA2
        (,)
        (get (fromBackendKey (SqlBackendKey workId)))
        ( select $
            from $ \(work `InnerJoin` movement) -> do
              E.on (work ^. WorkId ==. movement ^. MovementWorkId)
              where_ (work ^. WorkId ==. valkey workId)
              orderBy [asc (movement ^. MovementNumber)]
              pure movement
        )
  pure
    ( maybe emptyArray (toJSON . workInstrumentation) work
    , array (map jsonMovement movements)
    )
  where
    jsonMovement (Entity key Movement {..}) =
      object ["text" .= (tshow movementNumber <> ". " <> movementName), "value" .= key]

getEntriesR :: Int64 -> Handler Value
getEntriesR workId = do
  entries <- runDB $
    select $
      from $ \(user `InnerJoin` entry `InnerJoin` movement `InnerJoin` oauth) -> do
        E.on (entry ^. EntryUploadedBy ==. user ^. UserId)
        E.on (entry ^. EntryMovementId ==. movement ^. MovementId)
        E.on (oauth ^. OAuthUserUserId ==. user ^. UserId)
        where_ (movement ^. MovementWorkId ==. valkey workId)
        orderBy
          [ asc (movement ^. MovementNumber)
          , asc (entry ^. EntryPart)
          , asc (entry ^. EntryMeasure_start)
          ]
        pure
          ( movement ^. MovementNumber
          , movement ^. MovementName
          , movement ^. MovementId
          , entry ^. EntryPart
          , entry ^. EntryMeasure_start
          , entry ^. EntryMeasure_end
          , entry ^. EntryDescription
          , user
          , oauth ^. OAuthUserName
          , entry ^. EntryCreatedAt
          , entry ^. EntryId
          )
  pure . array $
    map
      ( \( E.Value movementNum
          , E.Value movement
          , E.Value movementId
          , E.Value part
          , E.Value start
          , E.Value end
          , E.Value description
          , (Entity _ user)
          , E.Value oauth
          , E.Value time
          , E.Value entryId
          ) ->
            -- TODO: simplify this query to use formatUsername below
            let u = case userType user of
                  Email -> takeWhile (/= '@') $ userIdent user
                  OAuth -> fromMaybe "Anonymous" oauth
             in object
                  [ "movement" .= (tshow movementNum <> ". " <> movement)
                  , "part" .= part
                  , "measures" .= (tshow start <> " - " <> tshow end)
                  , "start_measure" .= start
                  , "description" .= description
                  , "uploaded_by" .= object ["user" .= u, "time" .= time]
                  , "movement_id" .= movementId
                  , "entry_id" .= entryId
                  ]
      )
      entries

formatUsername :: Entity User -> Handler Text
formatUsername (Entity userId user) = case userType user of
  Email -> pure $ takeWhile (/= '@') $ userIdent user
  OAuth -> do
    oauth <- runDB (getBy (UniqueOAuthUserId userId))
    pure case oauth of
      Just (Entity _ o) -> fromMaybe "Anonymous" $ oAuthUserName o
      _ -> "Anonymous"

getMusicXMLR :: Int64 -> Handler LText
getMusicXMLR entryId = entryMusicxml <$> runDB (get404 (toSqlKey entryId))
