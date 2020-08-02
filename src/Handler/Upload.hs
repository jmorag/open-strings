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
  (composers :: [Entity Composer]) <- runDB $ selectList [] [Asc ComposerFull_name]
  defaultLayout do
    addScript (StaticR js_opensheetmusicdisplay_min_js)
    addScript (StaticR js_fingeringeditor_js)
    addVueDev
    uploadId <- newIdent
    composerId <- newIdent
    workId <- newIdent
    movementId <- newIdent
    renderId <- newIdent
    $(widgetFile "upload")

addVueDev :: WidgetFor App ()
addVueDev = do
  addStylesheetRemote "https://cdn.materialdesignicons.com/5.3.45/css/materialdesignicons.min.css"
  addStylesheetRemote "https://use.fontawesome.com/releases/v5.2.0/css/all.css"
  addScriptRemote "https://cdn.jsdelivr.net/npm/vue/dist/vue.js"
  -- addStylesheetRemote "https://unpkg.com/buefy/dist/buefy.min.css"
  -- addScriptRemote "https://unpkg.com/buefy/dist/buefy.min.js"
