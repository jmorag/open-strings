module Import
  ( module Import,
  )
where

import Data.Char
import qualified Data.Text as T
import Foundation as Import
import Import.NoFoundation as Import
import Model as Import

addAutocomplete :: WidgetFor App ()
addAutocomplete = do
  addScript (StaticR js_autocomplete_js)
  addStylesheet (StaticR css_autocomplete_css)

addUnderscores :: Text -> Text
addUnderscores = T.map \c -> if isSpace c then '_' else c

replaceUnderscores :: Text -> Text
replaceUnderscores = T.map \case '_' -> ' '; c -> c
