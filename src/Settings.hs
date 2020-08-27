{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson
  ( (.!=),
    (.:?),
    Result (..),
    fromJSON,
    withObject,
  )
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Database.Persist.URL
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util
  ( WidgetFileSettings (..),
    widgetFileNoReload,
    widgetFileReload,
  )

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
  { -- | Directory from which to serve static files.
    appStaticDir :: String,
    -- | Configuration settings for accessing the database.
    appDatabaseConf :: PostgresConf,
    -- | Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    appRoot :: Maybe Text,
    -- | Host/interface the server should bind to.
    appHost :: HostPreference,
    -- | Port to listen on
    appPort :: Int,
    -- | Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    appIpFromHeader :: Bool,
    -- | Use detailed request logging system
    appDetailedRequestLogging :: Bool,
    -- | Should all log messages be displayed?
    appShouldLogAll :: Bool,
    -- | Use the reload version of templates
    appReloadTemplates :: Bool,
    -- | Assume that files in the static dir may change after compilation
    appMutableStatic :: Bool,
    -- | Perform no stylesheet/script combining
    appSkipCombining :: Bool,
    -- | Use development version of Vue
    appVueDevel :: Bool,
    -- Example app-specific configuration values.

    -- | Copyright text to appear in the footer of the page
    appCopyright :: Text,
    -- | Google Analytics code
    appAnalytics :: Maybe Text,
    -- | Indicate if auth dummy login should be enabled.
    appAuthDummyLogin :: Bool,
    -- | Google oauth2 params
    appGoogleOauthClientId :: Text,
    appGoogleOauthClientSecret :: Text,
    appGithubOauthClientId :: Text,
    appGithubOauthClientSecret :: Text
  }


instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    let defaultDev =
#ifdef DEVELOPMENT
          True
#else
          False
#endif
    appStaticDir <- o .: "static-dir"
    appDatabaseConf <- do
      pool <- o .: "database-pool-size"
      url <- o .: "database-url"
      fromDatabaseUrl pool (url :: Text)
    appRoot <- o .:? "approot"
    appHost <- fromString <$> o .: "host"
    appPort <- o .: "port"
    appIpFromHeader <- o .: "ip-from-header"

    dev <- o .:? "development" .!= defaultDev

    appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
    appShouldLogAll <- o .:? "should-log-all" .!= dev
    appReloadTemplates <- o .:? "reload-templates" .!= dev
    appMutableStatic <- o .:? "mutable-static" .!= dev
    appSkipCombining <- o .:? "skip-combining" .!= dev
    appVueDevel <- o .:? "vue-devel" .!= dev

    appCopyright <- o .: "copyright"
    appAnalytics <- o .:? "analytics"

    appAuthDummyLogin <- o .:? "auth-dummy-login" .!= dev

    appGoogleOauthClientId <- o .: "google-oauth2-client-id"
    appGoogleOauthClientSecret <- o .: "google-oauth2-client-secret"
    appGithubOauthClientId <- o .: "github-oauth2-client-id"
    appGithubOauthClientSecret <- o .: "github-oauth2-client-secret"

    return AppSettings {..}


-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
