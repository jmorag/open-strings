module Yesod.Auth.OAuth2.Google.Custom (customOauth2GoogleScoped, oauth2GoogleScopedWidget) where

import Yesod.Auth
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.Dispatch
import Yesod.Auth.OAuth2.Prelude
import Yesod.Core.Widget
import Prelude

pluginName :: Text
pluginName = "google"

customOauth2GoogleScoped ::
  forall m.
  YesodAuth m =>
  [Text] ->
  Text ->
  Text ->
  (Route m -> WidgetFor m ()) ->
  AuthPlugin m
customOauth2GoogleScoped scopes clientId clientSecret mkWidget =
  AuthPlugin
    pluginName
    (dispatchAuthRequest pluginName oauth2 fetchAccessToken getCreds)
    login
  where
    oauth2 =
      OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = Just clientSecret
        , oauthOAuthorizeEndpoint =
            "https://accounts.google.com/o/oauth2/auth"
              `withQuery` [scopeParam " " scopes]
        , oauthAccessTokenEndpoint =
            "https://www.googleapis.com/oauth2/v3/token"
        , oauthCallback = Nothing
        }

    login tm = mkWidget (tm (oauth2Url pluginName))

getCreds :: FetchCreds m
getCreds manager token = do
  (User userId, userResponse) <-
    authGetProfile
      pluginName
      manager
      token
      "https://www.googleapis.com/oauth2/v3/userinfo"

  pure
    Creds
      { credsPlugin = "google"
      , credsIdent = userId
      , credsExtra = setExtra token userResponse
      }

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
        (User userId, userResponse) <- authGetProfile
            pluginName
            manager
            token
            "https://www.googleapis.com/oauth2/v3/userinfo"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = userId
            , credsExtra = setExtra token userResponse
            }
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = Just clientSecret
        , oauthOAuthorizeEndpoint =
            "https://accounts.google.com/o/oauth2/auth"
                `withQuery` [scopeParam " " scopes]
        , oauthAccessTokenEndpoint =
            "https://www.googleapis.com/oauth2/v3/token"
        , oauthCallback = Nothing
        }
