{-# LANGUAGE OverloadedStrings #-}

module TestTimeFormat where


import Test.HUnit
import Data.Time.LocalTime (utc)
import qualified Column as C


tests = test
  [
    "format epoch begigging" ~:
      C.formatTimestamp utc 0 ~=? "00:00:00 01.01.1970"
  ]
