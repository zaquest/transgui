{-
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
-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import Data.GI.Base

main :: IO ()
main = do
  Gtk.init Nothing
  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit
  button <- new Gtk.Button [ #label := "Click me" ]
  on button #clicked (set button [ #sensitive := False,
                                   #label := "Thanks for clicking me" ])
  #add win button
  #showAll win
  Gtk.main
