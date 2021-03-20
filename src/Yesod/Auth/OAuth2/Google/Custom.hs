module Yesod.Auth.OAuth2.Google.Custom (oauth2GoogleScopedWidget) where

import Yesod.Auth
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.Prelude
import Yesod.Core.Widget
import Prelude

-- TODO: upstream this into yesod-auth-oauth2. It's almost entirely
-- copy-pasted from their source

pluginName :: Text
pluginName = "google"

newtype User = User Text

instance FromJSON User where
  parseJSON =
    withObject "User" $ \o ->
      User
        -- Required for data backwards-compatibility
        <$> (("google-uid:" <>) <$> o .: "sub")

oauth2GoogleScopedWidget :: YesodAuth m => WidgetFor m () -> [Text] -> Text -> Text -> AuthPlugin m
oauth2GoogleScopedWidget widget scopes clientId clientSecret =
  authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        "https://www.googleapis.com/oauth2/v3/userinfo"

    pure
      Creds
        { credsPlugin = pluginName
        , credsIdent = userId
        , credsExtra = setExtra token userResponse
        }
  where
    oauth2 =
      OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint =
            "https://accounts.google.com/o/oauth2/auth"
              `withQuery` [scopeParam " " scopes]
        , oauthAccessTokenEndpoint =
            "https://www.googleapis.com/oauth2/v3/token"
        , oauthCallback = Nothing
        }
