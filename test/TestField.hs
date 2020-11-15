{-# LANGUAGE OverloadedStrings #-}
module TestField where

import Test.HUnit
import Control.Monad (void)
import Some
import qualified Field as F
import RPC
import qualified Response
import TestDaemon


tests = test
  [
    "allFields is sorted by idx" ~:
      mapWithSome F.allFields F.idx ~=? [0..F.total-1],

    "nubFields removes duplicates" ~:
      let
        fieldsWithDuplicates =
          [ mkSome F.id
          , mkSome F.id
          , mkSome F.name
          , mkSome F.id
          , mkSome F.rateUpload
          , mkSome F.name
          , mkSome F.rateUpload
          ]
        uniqueFields =
          [ mkSome F.id
          , mkSome F.name
          , mkSome F.rateUpload
          ]
      in F.nub fieldsWithDuplicates ~=? uniqueFields,

    -- TODO: replace me using withTransmission
    -- testTorrentGetAll,

    testWithTransmission
  ]


testSettings :: RPC.Settings
testSettings = RPC.Settings
  { rpcAddress = "http://192.168.0.100:9091/transmission/rpc"
  }


rpcRun :: RPC.RPC a -> IO a
rpcRun action = do
  rpcData <- RPC.init testSettings
  RPC.run rpcData action


rpcRunWithUrl :: String -> RPC.RPC a -> IO a
rpcRunWithUrl url action = do
  rpcData <- RPC.init $ RPC.Settings { rpcAddress = url }
  RPC.run rpcData action


testTorrentGetAll = "`torrentGet AllTorrents` returns `torrents`" ~: do
  args <- rpcRun (RPC.torrentGet RPC.AllTorrents ["id", "name"])

  let torrents = Response.torrents args
  assertBool "Should receive torrent list" (not $ null torrents)

  let removed = Response.removed args
  assertBool "Should not recieve any removed ids" (null removed)


testWithTransmission = "new daemon instance has no torrents" ~:
  void . withTransmission $ \url -> do
    let torrentGet = RPC.torrentGet RPC.AllTorrents ["id", "name"]
    args <- rpcRunWithUrl url torrentGet

    let torrents = Response.torrents args
    assertBool "Should receive torrent list" (null torrents)

    let removed = Response.removed args
    assertBool "Should not recieve any removed ids" (null removed)
