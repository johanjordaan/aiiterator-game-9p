import Test.Hspec

import PositionSpec
import BoundSpec
import BoardSpec
import ActionSpec
import GameSpec
import ServerSpec

main :: IO ()
main = hspec $ do
  boundSpec
  positionSpec
  boardSpec
  actionSpec
  gameSpec
  serverSpec
