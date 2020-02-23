module TestField where

import Test.HUnit
import Data.Int (Int32)
import Some
import qualified Field as F


tests = test
  [
    "allFields is sorted by idx" ~:
      (mapWithSome F.allFields F.idx) ~=? [0..F.total-1]
  ]
