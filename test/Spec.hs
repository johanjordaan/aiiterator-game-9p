import Test.Hspec

import BoardSpec
import ActionSpec
import CoordSpec
import GameSpec

main :: IO ()
main = hspec $ do
  boardSpec
  actionSpec
  coordSpec
  gameSpec
