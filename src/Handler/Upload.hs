{-# LANGUAGE TemplateHaskell #-}
module Handler.Upload where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

postUploadR :: Handler Html
postUploadR = undefined

-- uploadForm :: Form (Text, Text)
-- uploadForm = renderBootstrap3 BootstrapBasicForm $ (,) <$>
--   areq (jqueryAutocompleteField ComposersR) "Composer" Nothing <*>
--   areq (textField) "Work" Nothing

getUploadR :: Handler Html
getUploadR = do
  -- (widget, enctype) <- generateFormPost uploadForm
  (composers :: [Entity Composer]) <- runDB $ selectList [] []
  defaultLayout do
    addScriptRemote "https://cdn.jsdelivr.net/npm/vue/dist/vue.js"
    uploadId <- newIdent
    composerId <- newIdent
    workId <- newIdent
    movementId <- newIdent
    $(widgetFile "upload")



-- addJquery = do
--   yesod <- getYesod
--   addScriptEither $ urlJqueryJs yesod
--   addScriptEither $ urlJqueryUiJs yesod
--   addStylesheetEither $ urlJqueryUiCss yesod
