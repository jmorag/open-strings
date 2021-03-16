module Import (
  module Import,
) where

import Foundation as Import
import Import.NoFoundation as Import

addAutocomplete :: WidgetFor App ()
addAutocomplete = do
  addScript (StaticR js_autocomplete_js)
  addStylesheet (StaticR css_autocomplete_css)
  addScript (StaticR js_autocomplete_work_js)
