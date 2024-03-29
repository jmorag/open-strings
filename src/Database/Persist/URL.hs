module Database.Persist.URL (
  fromDatabaseUrl,
) where

import ClassyPrelude hiding (uncons)
import Control.Monad.Fail (MonadFail, fail)
import Data.ByteString (uncons)
import qualified Data.ByteString.Char8 as Char8
import Data.String.Conversions (ConvertibleStrings (..))
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Database.Persist.Postgresql (PostgresConf (..))
import URI.ByteString (
  Authority (..),
  Host (..),
  Port (..),
  Scheme (..),
  URIRef (..),
  UserInfo (..),
  parseURI,
  strictURIParserOptions,
 )

-- | Build a @'PostgresConf'@ by parsing a database URL String
fromDatabaseUrl ::
  (ConvertibleStrings s ByteString, Monad m, MonadFail m) =>
  Int ->
  s ->
  m PostgresConf
fromDatabaseUrl size url = do
  uri <- abortLeft $ parseURI strictURIParserOptions $ toStrictByteString url
  auth <- abortNothing "authority" $ uriAuthority uri
  userInfo <- abortNothing "user info" $ authorityUserInfo auth
  port <- abortNothing "port" $ authorityPort auth
  dbName <- abortNothing "path" $ snd <$> uncons (uriPath uri)
  unless (schemeBS (uriScheme uri) == "postgres") $
    fail "HEROKU_POSTGRESQL_BLACK_URL has unknown scheme"

  return
    PostgresConf
      { pgConnStr =
          "user=" <> uiUsername userInfo
            <> " password="
            <> uiPassword userInfo
            <> " host="
            <> hostBS (authorityHost auth)
            <> " port="
            <> Char8.pack (show $ portNumber port)
            <> " dbname="
            <> dbName
      , pgPoolSize = size
      }

abortLeft :: (MonadFail m, Show e) => Either e b -> m b
abortLeft = either (fail . ("HEROKU_POSTGRESQL_BLACK_URL failed to parse: " <>) . show) return

abortNothing :: MonadFail m => String -> Maybe a -> m a
abortNothing s = maybe (fail $ "HEROKU_POSTGRESQL_BLACK_URL is missing " <> s) return
