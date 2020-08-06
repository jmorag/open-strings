{-# LANGUAGE TemplateHaskell #-}

module Model.Parts where

import ClassyPrelude.Yesod
import Dhall

data Solo = Solo | Tutti
  deriving (Show, Eq, Read, Ord, Generic)

data StringInstrument = Violin | Viola | Cello | Bass
  deriving (Show, Eq, Read, Ord, Generic)

data Part = Part
  { instrument :: StringInstrument,
    solo :: Solo,
    -- | Part 0 denotes that it's the only instrument in the score, i.e.
    -- violin concerto soloist is Part Violin Solo 0
    -- whereas the first violinist in a quartet is Part Violin Solo 1
    part_num :: Natural
  }
  deriving (Show, Eq, Read, Ord, Generic)

derivePersistField "Part"

instance FromDhall Part

instance FromDhall StringInstrument

instance FromDhall Solo
