{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client
import Network.HTTP.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import RPC

main :: IO ()
main = do
  let settings = RPCSettings "http://192.168.0.100:9091/transmission/rpc"
  hdr <- runRPC settings doshit
  print hdr
