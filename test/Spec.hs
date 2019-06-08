import Test.Hspec

import ActionSpec
import CoordSpec
import GameSpec

main :: IO ()
main = hspec $ do
  actionSpec
  coordSpec
  gameSpec
