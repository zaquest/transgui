{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
module Store where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.GI.Base
import qualified GI.Gtk as Gtk

import Some
import Torrent (Torrent, TID)
import qualified Torrent as T
import Field (Field)
import qualified Field as F
import Response (TorrentGetArguments(..))


data Store = Store
  { store :: Gtk.ListStore
  , rows :: IORef (HashMap TID Gtk.TreeIter)
  }


update :: Store -> [Some Field] -> TorrentGetArguments -> IO ()
update store fields TorrentGetArguments{ torrents, removed } = do
  mapM_ (updateOrInsertTorrent store fields) torrents
  mapM_ (removeTorrent store) removed


updateOrInsertTorrent :: Store -> [Some Field] -> Torrent -> IO ()
updateOrInsertTorrent store@Store{ rows } fields torrent = do
  result <- HashMap.lookup tid <$> readIORef rows
  case result of
    Just iter -> updateTorrent store iter fields torrent
    Nothing -> insertTorrent store fields torrent
  where
    tid = T._id torrent


insertTorrent :: Store -> [Some Field] -> Torrent -> IO ()
insertTorrent Store{ store, rows } fields torrent = do
  let idxs = F.indices fields
  gvals <- F.gvalues fields torrent
  iter <- #insertWithValuesv store (-1) idxs gvals
  let tid = T._id torrent
  modifyIORef rows (HashMap.insert tid iter)


updateTorrent :: Store -> Gtk.TreeIter -> [Some Field] -> Torrent -> IO ()
updateTorrent Store{ store } iter fields torrent = do
  let idxs = F.indices fields
  gvals <- F.gvalues fields torrent
  #set store iter idxs gvals


removeTorrent :: Store -> TID -> IO ()
removeTorrent Store{ store, rows } tid = do
  result <- HashMap.lookup tid <$> readIORef rows
  case result of
    Just iter -> do
      #remove store iter
      modifyIORef rows (HashMap.delete tid)
    Nothing -> pure ()


empty :: IO Store
empty = do
  store <- new Gtk.ListStore []
  #setColumnTypes store (mapWithSome F.allFields F.gtype)
  rows <- newIORef HashMap.empty
  pure (Store store rows)
