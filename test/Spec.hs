import Test.Hspec

import Prelude
import qualified MusicXMLSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MusicXML" MusicXMLSpec.spec
