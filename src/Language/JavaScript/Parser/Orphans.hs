{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JavaScript.Parser.Orphans where

import ClassyPrelude
import Language.JavaScript.Parser.AST

deriving instance Generic JSAST

deriving instance Generic JSStatement

deriving instance Generic JSExpression

deriving instance Generic (JSCommaList a)

deriving instance Generic (JSCommaTrailingList a)

deriving instance Generic JSObjectProperty

deriving instance Generic JSArrayElement

deriving instance Functor JSCommaList

deriving instance Functor JSCommaTrailingList

deriving instance Foldable JSCommaList

deriving instance Foldable JSCommaTrailingList

deriving instance Traversable JSCommaList

deriving instance Traversable JSCommaTrailingList
