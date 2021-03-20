{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import qualified Data.Text as T
import Database.Esqueleto hiding (Value)
import qualified Database.Esqueleto as E
import Import hiding ((==.))
import Model.UserType
import Text.Julius

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  entries <- mostRecentUploads
  defaultLayout do
    setTitle "Welcome To OpenStrings!"
    addAutocomplete
    searchId <- newIdent
    tableId <- newIdent
    csrf <- getCSRF
    $(widgetFile "homepage")

mostRecentUploads :: Handler Value
mostRecentUploads = do
  entries <- runDB $
    select $
      from $ \(user `InnerJoin` entry `InnerJoin` movement `InnerJoin` work `InnerJoin` composer `InnerJoin` oauth) -> do
        E.on (entry ^. EntryUploadedBy ==. user ^. UserId)
        E.on (entry ^. EntryMovementId ==. movement ^. MovementId)
        E.on (movement ^. MovementWorkId ==. work ^. WorkId)
        E.on (work ^. WorkComposerId ==. composer ^. ComposerId)
        E.on (oauth ^. OAuthUserUserId ==. user ^. UserId)
        E.where_ (E.isNothing $ entry ^. EntryIsAlgorithm)
        orderBy [desc (entry ^. EntryCreatedAt)]
        limit 10
        pure
          ( composer ^. ComposerName
          , work ^. WorkTitle
          , movement ^. MovementNumber
          , movement ^. MovementName
          , entry ^. EntryPart
          , entry ^. EntryMeasure_start
          , entry ^. EntryMeasure_end
          , entry ^. EntryDescription
          , user
          , oauth ^. OAuthUserName
          , entry ^. EntryCreatedAt
          , work ^. WorkId
          , entry ^. EntryId
          )
  pure . array $
    map
      ( \( E.Value composer
          , E.Value title
          , E.Value movementNum
          , E.Value movement
          , E.Value part
          , E.Value start
          , E.Value end
          , E.Value description
          , (Entity _ user)
          , E.Value oauth
          , E.Value time
          , E.Value workId
          , E.Value entryId
          ) ->
            let u = case userType user of
                  Email -> takeWhile (/= '@') $ userIdent user
                  OAuth -> fromMaybe "Anonymous" oauth
                work =
                  T.takeWhile (/= ',') composer
                    <> ": "
                    <> replaceUnderscores title
                    <> case movementNum of
                      0 -> ""
                      _ ->
                        " - "
                          <> tshow movementNum
                          <> ". "
                          <> movement
             in object
                  [ "work" .= work
                  , "part" .= part
                  , "measures" .= (tshow start <> " - " <> tshow end)
                  , "description" .= description
                  , "uploaded_by" .= object ["user" .= u, "time" .= time]
                  , "work_id" .= workId
                  , "entry_id" .= entryId
                  ]
      )
      entries
