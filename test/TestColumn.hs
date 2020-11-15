module TestColumn where

import Test.HUnit
import Some
import qualified Field as F
import qualified Column as C


tests = test
  [
    "collectFields removes duplicates" ~:
      let
        columnsWithDuplicates =
          [ C.id
          , C.id
          , C.name
          , C.id
          , C.uploadSpeed
          , C.name
          , C.uploadSpeed
          ]
        uniqueMatchingFields =
          [ mkSome F.id
          , mkSome F.name
          , mkSome F.rateUpload
          ]
      in C.collectFields columnsWithDuplicates ~=? uniqueMatchingFields
  ]
