{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms, NamedFieldPuns #-}
module UI where

import Control.Monad (void)
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, newMVar, readMVar)
import GI.GLib.Functions (timeoutAddSeconds)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT, pattern SOURCE_CONTINUE)
import Some
import Field (Field)
import qualified Field as F
import Column (Column)
import qualified Column as C
import Torrent (Torrent)

import Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk


data Data = Data
  { uiApp :: Gtk.Application
  , uiBuilder :: MVar Gtk.Builder
  , uiStore :: Gtk.ListStore
  , uiColumns :: MVar [Column]
  }


newtype UI a = UI { runUI :: ReaderT Data IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


mkListStore :: IO Gtk.ListStore
mkListStore = do
  store <- new Gtk.ListStore []
  #setColumnTypes store (mapWithSome F.allFields F.gtype)
  pure store


getBuilderObj :: forall o'
               . GObject o'
              => Gtk.Builder
              -> Text
              -> (ManagedPtr o' -> o')
              -> IO (Maybe o')
getBuilderObj builder name gtkConstr = #getObject builder name >>= \case
  Just obj -> castTo gtkConstr obj
  Nothing -> do
    T.putStrLn $ "Object named '" <> name <> "' could not be found."
    return Nothing


quitAction :: Gtk.Application -> IO ()
quitAction app = do
  action <- new Gio.SimpleAction [ #name := "quit" ]
  on action #activate $ const (#quit app)
  Gio.actionMapAddAction app action


initTreeView :: Gtk.TreeView -> [Column] -> IO ()
initTreeView tv cols = mapM_ (C.mkTreeViewColumn tv) cols


activateApp :: Gtk.ListStore -> MVar Gtk.Builder -> Gtk.Application -> IO ()
activateApp store mbuilder app = do
  -- It is impossible to create builder before app activation
  builder <- new Gtk.Builder []
  #addFromFile builder "ui/main.glade"

  Just window <- getBuilderObj builder "window" Gtk.ApplicationWindow
  set window [ #application := app ]

  Just torrentList <- getBuilderObj builder "torrent-list" Gtk.TreeView
  set torrentList [ #model := store ]

  initTreeView torrentList C.allColumns

  #showAll window

  putMVar mbuilder builder


updateStoreCb :: Data -> ([Text] -> IO [Torrent]) -> IO Bool
updateStoreCb (Data {uiColumns, uiStore}) getTorrents = do
  cols <- readMVar uiColumns
  let fields = C.collectFields cols
  torrents <- getTorrents (F.keys fields)
  updateStore uiStore fields torrents
  pure SOURCE_CONTINUE


init :: ([Text] -> IO [Torrent]) -> IO Data
init getTorrents = do
  app <- new Gtk.Application
    [ #applicationId := "zaquest.transgui"
    , #flags := [ Gio.ApplicationFlagsFlagsNone ]
    ]
  mbuilder <- newEmptyMVar
  mcolumns <- newMVar C.allColumns
  store <- mkListStore
  void (on app #activate (activateApp store mbuilder app))
  quitAction app
  let datum = (Data app mbuilder store mcolumns)
  _ <- updateStoreCb datum getTorrents
  timeoutAddSeconds PRIORITY_DEFAULT 5 (updateStoreCb datum getTorrents)
  pure datum


updateStore :: Gtk.ListStore -> [Some Field] -> [Torrent] -> IO ()
updateStore store fields torrents = do
  #clear store
  mapM_ (addTorrent store fields) torrents


addTorrent :: Gtk.ListStore -> [Some Field] -> Torrent -> IO ()
addTorrent store fields torrent = do
  let idxs = F.indices fields
  gvals <- F.gvalues fields torrent
  void $ #insertWithValuesv store (-1) idxs gvals


asks :: (Data -> a) -> UI a
asks f = UI (Reader.asks f)


ask :: UI Data
ask = asks id


start :: UI ()
start = do
  app <- asks uiApp
  liftIO $ void (Gio.applicationRun app Nothing)


run :: Data -> UI a -> IO a
run uiData (UI action) = Reader.runReaderT action uiData


-- main :: IO ()
-- main = do
--     app <- new Gtk.Application
--       [ #applicationId := "zaquest.transgui"
--       , #flags := [ Gio.ApplicationFlagsFlagsNone ]
--       ]
--     void (on app #activate (activateApp app))
--     void (Gio.applicationRun app Nothing)
