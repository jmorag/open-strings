module Handler.Upload where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

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

getUploadR :: Handler Html
getUploadR = defaultLayout do
  [whamlet|
<div #upload-music>
  <script type="module" src=@{StaticR js_upload_js}>
|]
