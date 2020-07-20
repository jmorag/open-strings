module Main where

import Control.Lens
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson.Lens
import Data.Data.Lens
import Data.Generics.Product
import Data.Generics.Sum
import Data.String.Conversions
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql
import Import
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Orphans ()
import Network.HTTP.Simple
import Text.HTML.DOM
import Text.RawString.QQ
import Text.XML.Lens

htmlOf :: (MonadUnliftIO m) => Request -> m Document
htmlOf = fmap (parseLBS . getResponseBody) . httpLBS

getWorksHtml :: (MonadUnliftIO m, MonadThrow m) => Entity Composer -> ReaderT SqlBackend m ()
getWorksHtml composer = do
  doc <- htmlOf (parseRequest_ (cs (composerImslp_url (entityVal composer))))
  let scriptPrefix =
        [r|if(typeof catpagejs=='undefined')catpagejs={};$.extend(catpagejs,{"p1"|]
      workIds =
        doc ^.. root
          . script (scriptPrefix `isPrefixOf`)
          . stringLiterals
          . filtered (\s -> not (T.null s) && T.head s /= '|')
          . to (T.dropWhileEnd (/= ')'))
  pooledForConcurrently_ workIds \work -> do
    let pieceLink = "https://imslp.org/wiki/" <> urlEncode False (cs work)
    (movements, instrumentation) <- getMovementsAndInstrumentation =<< parseRequest (cs pieceLink)
    void $ insert $ Work work pieceLink movements instrumentation (entityKey composer)
  say $ "Got works for " <> composerFull_name (entityVal composer)

getMovementsAndInstrumentation :: (MonadUnliftIO m) => Request -> m ([Text], Maybe Text)
getMovementsAndInstrumentation pieceLink = do
  doc <- htmlOf pieceLink
  let movements =
        let tr = doc ^? root . deep (el "tr") . taking 1 (filtered (\tr' -> Just "Movements/Sections" == tr' ^? deep (el "span" . text)))
         in map T.strip (tr ^.. _Just . deep (el "li" . text) <|> tr ^.. _Just . deep (el "dd" . text))
      instrumentation =
        doc ^? root
          . deep (el "tr")
          . filtered (\tr -> tr ^. plate . el "th" . text == "Instrumentation")
          . plate
          . el "td"
          . text
  pure (movements, instrumentation)

composerHtml :: (MonadThrow m, MonadUnliftIO m) => m Document
composerHtml =
  parseLBS . getResponseBody
    <$> httpLBS "https://imslp.org/wiki/Category:Composers"

getComposers :: Document -> [Composer]
getComposers doc =
  doc ^? root
    . deep (el "div" . attributeIs "class" "body")
    . script (const True)
    ^.. _Just
    . stringLiterals
    . to (\composer -> Composer composer (composerUrl composer))

script :: (Text -> Bool) -> Fold Text.XML.Lens.Element JSAST
script p =
  deep (el "script") . text
    . filtered p
    . to (flip parse "" . unpack)
    . _Right

stringLiterals :: Traversal' JSAST Text
stringLiterals =
  _Ctor @"JSAstProgram"
    . typed @[JSStatement]
    . ix 1
    . _Ctor @"JSMethodCall"
    . typed @(JSCommaList JSExpression)
    . traversed
    . deepOf uniplate (_Ctor @"JSStringLiteral")
    . typed @String
    . _String

composerUrl :: Text -> ByteString
composerUrl composer =
  "https://imslp.org/wiki/Category:" <> (urlEncode False . cs $ composer)

populateDB :: IO ()
populateDB = runStderrLoggingT $ withPostgresqlPool "dbname=fingerdb" 10 $
  \pool -> liftIO $ flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    composers <- composerHtml >>= traverse insertEntity . getComposers
    traverse_ getWorksHtml composers

main = populateDB
