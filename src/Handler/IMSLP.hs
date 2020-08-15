module Handler.IMSLP where

import Control.Lens hiding ((.=))
import Control.Lens.Regex.Text
import Data.Char
import Data.Data.Lens
import qualified Data.Text as T
import Import
import Network.HTTP.Simple
import Text.HTML.DOM
import Text.XML.Lens

htmlOf :: (MonadUnliftIO m) => Request -> m Document
htmlOf = fmap (parseLBS . getResponseBody) . httpLBS

findMovements :: Document -> [Text]
findMovements doc =
  doc
    ^.. root
      . deep (el "tr")
      . taking 1 (filtered (\tr -> Just "Movements/Sections" == tr ^? deep (el "span" . text)))
      . failing (deep (el "li" . text)) (deep (el "dd" . text))
      . to (T.strip . dropKeys . dropRomanNumerals . T.strip . fixSpaces)
  where
    fixSpaces = T.map \c -> if isSpace c then ' ' else c
    dropRomanNumerals = T.dropWhile (`elem` ("IVX." :: [Char]))
    dropKeys = set ([regex|\s\([A-G][^\s]*\s(minor|major)\)|] . match) ""

findInstrumentation :: Document -> [Text]
findInstrumentation doc =
  doc
    ^.. root
      . deep (el "tr")
      . filtered (\tr -> tr ^? deep (el "th" . text) . to T.strip == Just "Instrumentation")
      . deep (el "td")
      -- Biplate is deeply magical.
      -- Gets all of the text like things from the td element
      . biplate
      . [regex|((solo|tutti)\s*)?\b(violin|viola|cello|double\sbass)\b(\s+[1-9]\d*)?|strings|quartet|]
      . match
      -- There are a lot of non-string instruments with bass in their names, but we only
      -- care about string double basses
      . to (\case "double bass" -> "bass"; instr -> instr)

getIMSLPR :: (MonadThrow m, MonadUnliftIO m) => String -> m Value
getIMSLPR imslp = do
  request <- parseRequest imslp
  doc <- htmlOf request
  let imslp' = T.pack imslp
      title =
        decodeUrl
          . T.strip
          . T.map (\case '_' -> ' '; c -> c)
          . T.drop (T.length "https://imslp.org/wiki/")
          . T.dropEnd 1
          . T.dropWhileEnd (/= '(')
          $ imslp'
      composer =
        decodeUrl
          . T.strip
          . T.map (\case '_' -> ' '; c -> c)
          . T.dropEnd 1
          . T.takeWhileEnd (/= '(')
          $ imslp'
  pure $
    object
      [ "instrumentation" .= findInstrumentation doc,
        "movements" .= findMovements doc,
        "composer" .= composer,
        "composer_url" .= ("https://imslp.org.wiki/Category:" <> composer),
        "title" .= title
      ]
  where
    decodeUrl = decodeUtf8 . urlDecode False . encodeUtf8
