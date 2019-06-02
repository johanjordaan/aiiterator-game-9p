import Test.Hspec

import CoordSpec
import GameSpec

main :: IO ()
main = hspec $ do
  coordSpec
  gameSpec
