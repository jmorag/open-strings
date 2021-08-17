{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parts where

import ClassyPrelude.Yesod
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Validation

data Solo = Solo | Tutti
  deriving (Show, Eq, Read, Ord, Generic)

data StringInstrument = Violin | Viola | Cello | Bass
  deriving (Show, Eq, Read, Ord, Generic)

data Part = Part
  { instrument :: StringInstrument
  , -- | Parts default to solo unless tutti is indicated
    solo :: Solo
  , -- | Part 0 denotes that it's the only instrument in the score, i.e.
    -- violin concerto soloist is Part Violin Solo 0
    -- whereas the first violinist in a quartet is Part Violin Solo 1
    part_num :: Natural
  }
  deriving (Show, Eq, Read, Generic)

-- We can't just switch the order of the fields in Part and use the derived instance
-- because it breaks the database instance
instance Ord Part where
  compare p1 p2 =
    compare (solo p1) (solo p2)
      <> compare (instrument p1) (instrument p2)
      <> compare (part_num p1) (part_num p2)

instance ToJSON Part where
  toJSON = String . showPart

showPart :: Part -> Text
showPart Part {..} =
  case solo of Solo -> ""; Tutti -> "Tutti "
    <> tshow instrument
    <> case part_num of 0 -> ""; _ -> " " <> tshow part_num

-- Used only in upload-fingerings form
instance FromJSON Part where
  parseJSON =
    withText
      "Part"
      (maybe (parseFail "Unable to parse part") pure . readPart)

instance PathPiece Part where
  fromPathPiece = readPart
  toPathPiece = showPart

readPart :: Text -> Maybe Part
readPart p = case T.words p of
  [instr] -> readMay instr <&> (\i -> Part i Solo 0)
  ["Tutti", instr] -> readMay instr <&> (\i -> Part i Tutti 0)
  [instr, n] -> Part <$> readMay instr <*> pure Solo <*> readMay n
  ["Tutti", instr, n] -> Part <$> readMay instr <*> pure Tutti <*> readMay n
  _ -> Nothing

-- Used only in add-work form
instance {-# OVERLAPS #-} FromJSON (Set Part) where
  parseJSON =
    withArray "Set Part" $
      fmap (Set.fromList . concat)
        -- can't use withText here because it short circuits the validation applicative
        . ( hoistValidation
              . traverse
                ( \case
                    (String p) -> parseParts p
                    invalid -> failure ("Expected String, got " <> show invalid)
                )
          )

parseParts :: Text -> Validation (NonEmpty String) [Part]
parseParts p = case fmap (over (ix 0) charToUpper) (words p) of
  ["Quartet"] ->
    pure
      [ Part Violin Solo 1
      , Part Violin Solo 2
      , Part Viola Solo 0
      , Part Cello Solo 0
      ]
  ["Strings"] ->
    pure
      [ Part Violin Tutti 1
      , Part Violin Tutti 2
      , Part Viola Tutti 0
      , Part Cello Tutti 0
      ]
  single -> (: []) <$> parsePart single

parsePart :: [Text] -> Validation (NonEmpty String) Part
parsePart = \case
  ["Tutti", instrument] -> part Tutti instrument "0"
  ["Tutti", instrument, num] -> part Tutti instrument num
  ["Solo", instrument] -> part Solo instrument "0"
  ["Solo", instrument, num] -> part Solo instrument num
  [instrument] -> part Solo instrument "0"
  [instrument, num] -> part Solo instrument num
  other -> failure ("Unable to parse part " <> T.unpack (T.unwords other))
  where
    part :: Solo -> Text -> Text -> Validation (NonEmpty String) Part
    part solo instr num = do
      i <- parseInstrument instr
      n <- parsePartNum num
      pure $ Part i solo n
      where
        validateRead fn_err x = maybeToSuccess (fn_err x :| []) (readMay x)
        parsePartNum = validateRead (\n -> "Unknown part number " <> show n)
        parseInstrument = validateRead (\i -> "Unknown instrument " <> unpack i)

hoistValidation :: Validation (NonEmpty String) a -> Parser a
hoistValidation = validation (parseFail . unlines) pure

derivePersistField "Part"
