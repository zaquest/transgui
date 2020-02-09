{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UI where

import Data.Monoid ((<>))
import Control.Monad (void)
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import System.Environment (getArgs)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.Int
import Some
import Field (Field)
import qualified Field as F

import Data.GI.Base
import Data.GI.Base.GType
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk


data Data = Data
  { uiApp :: Gtk.Application
  , uiBuilder :: MVar Gtk.Builder
  , uiStore :: Gtk.ListStore
  }


newtype UI a = UI { runUI :: ReaderT Data IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


printHello :: Text -> IO ()
printHello t = T.putStrLn $ "Hello from " <> t <> "."


printQuit :: Gtk.Application -> Text -> IO ()
printQuit app t = do
  T.putStrLn $ "Quitting by " <> t <> "."
  #quit app
  return ()


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


-- Be aware that this function silently ignores absent names
-- connectBtnClick :: Gtk.Builder -> Text -> IO () -> IO ()
-- connectBtnClick builder name handler =
--   getBuilderObj builder name Gtk.Button >>= \case
--     Just button -> void (on button #clicked handler)
--     Nothing -> return ()


quitAction :: Gtk.Application -> IO ()
quitAction app = do
  action <- new Gio.SimpleAction [ #name := "quit" ]
  on action #activate $ const (#quit app)
  Gio.actionMapAddAction app action


activateApp :: Gtk.ListStore -> MVar Gtk.Builder -> Gtk.Application -> IO ()
activateApp store mbuilder app = do
  builder <- new Gtk.Builder []
  #addFromFile builder "ui/main.glade"

  Just window <- getBuilderObj builder "window" Gtk.ApplicationWindow
  set window [ #application := app ]

  n1 <- toGValue (1 :: Int64)
  s1 <- toGValue ("Row 1" :: Text)
  #insertWithValuesv store (-1) [0, 1] [n1, s1]
  n2 <- toGValue (2 :: Int64)
  s2 <- toGValue ("Row 2" :: Text)
  #insertWithValuesv store (-1) [0, 1] [n2, s2]
  n3 <- toGValue (3 :: Int64)
  s3 <- toGValue ("Row 3" :: Text)
  #insertWithValuesv store (-1) [0, 1] [n3, s3]

  idColumn <- new Gtk.TreeViewColumn [ #title := "ID" ]
  idRenderer <- new Gtk.CellRendererText []
  #packStart idColumn idRenderer True
  #addAttribute idColumn idRenderer "text" 0

  nameColumn <- new Gtk.TreeViewColumn [ #title := "Name" ]
  nameRenderer <- new Gtk.CellRendererText []
  #packStart nameColumn nameRenderer True
  #addAttribute nameColumn nameRenderer "text" 1

  Just torrentList <- getBuilderObj builder "torrent-list" Gtk.TreeView
  set torrentList [ #model := store ]
  #appendColumn torrentList idColumn
  #appendColumn torrentList nameColumn

  quitAction app

  -- on window #destroy $ printQuit app "windows close button"

  -- let name = "button1"
  -- connectBtnClick builder name $ do printHello name
  -- let name = "button2"
  -- connectBtnClick builder name $ do printHello name
  -- connectBtnClick builder "quit" $ printQuit app "quit button"

  #showAll window

  putMVar mbuilder builder


init :: IO Data
init = do
  app <- new Gtk.Application
    [ #applicationId := "zaquest.transgui"
    , #flags := [ Gio.ApplicationFlagsFlagsNone ]
    ]
  mbuilder <- newEmptyMVar
  store <- mkListStore
  void (on app #activate (activateApp store mbuilder app))
  pure (Data app mbuilder store)


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
