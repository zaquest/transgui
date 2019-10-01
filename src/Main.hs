{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client
import Network.HTTP.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified RPC
import qualified UI
import qualified Trans


main :: IO ()
-- main = UI.main
-- main = RPC.main
main = Trans.main
