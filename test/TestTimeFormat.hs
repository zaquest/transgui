{-# LANGUAGE OverloadedStrings #-}

module TestTimeFormat where


import Test.HUnit
import qualified Column as C


tests = test
  [
    "format epoch begigging" ~: do
      formatted <- C.formatTimestamp 0
      assertEqual "0 is 01.01.1970" "07:00:00 01.01.1970" formatted
  ]
