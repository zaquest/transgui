module TestColumn where

import Test.HUnit
import Data.Int (Int32)
import Some
import qualified Field as F
import qualified Column as C


tests = test
  [
    "collectFields removes duplicates" ~:
      (C.collectFields [C.id, C.id, C.name, C.id, C.uploadSpeed, C.name, C.uploadSpeed])
        ~=? 
          [This F.id, This F.name, This F.rateUpload]
  ]
