import Test.Hspec

import TestImport
import qualified Handler.CommonSpec
import qualified Handler.HomeSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Common" Handler.CommonSpec.spec
  describe "Home" Handler.HomeSpec.spec
