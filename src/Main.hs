{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (find)
import Network.HTTP.Client
import Network.HTTP.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

getHeader :: Response body -> HeaderName -> Maybe ByteString
getHeader resp hdr = snd <$> find ((hdr ==) . fst) (responseHeaders resp)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://192.168.0.100:9091/transmission/rpc"
  response <- httpLbs request manager

  print (getHeader response "X-Transmission-Session-Id")
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
