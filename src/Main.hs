{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client
import Network.HTTP.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified RPC

main :: IO ()
main = do
  let settings = RPC.Settings "http://192.168.0.100:9091/transmission/rpc"
  hdr <- RPC.run settings RPC.doshit
  print hdr
