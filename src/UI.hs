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

import Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk


data Data = Data
  { uiApp :: Gtk.Application
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
connectBtnClick :: Gtk.Builder -> Text -> IO () -> IO ()
connectBtnClick builder name handler =
  getBuilderObj builder name Gtk.Button >>= \case
    Just button -> void (on button #clicked handler)
    Nothing -> return ()


quitAction :: Gtk.Application -> IO ()
quitAction app = do
  action <- new Gio.SimpleAction [ #name := "quit" ]
  on action #activate $ const (#quit app)
  Gio.actionMapAddAction app action


activateApp :: Gtk.Application -> IO ()
activateApp app = do
  builder <- new Gtk.Builder []
  #addFromFile builder "ui/main.glade"

  Just window <- getBuilderObj builder "window" Gtk.ApplicationWindow
  set window [ #application := app ]

  quitAction app

  -- on window #destroy $ printQuit app "windows close button"

  -- let name = "button1"
  -- connectBtnClick builder name $ do printHello name
  -- let name = "button2"
  -- connectBtnClick builder name $ do printHello name
  -- connectBtnClick builder "quit" $ printQuit app "quit button"

  #showAll window


init :: IO Data
init = do
  app <- new Gtk.Application
    [ #applicationId := "zaquest.transgui"
    , #flags := [ Gio.ApplicationFlagsFlagsNone ]
    ]
  void (on app #activate (activateApp app))
  pure (Data app)


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


main :: IO ()
main = do
    app <- new Gtk.Application
      [ #applicationId := "zaquest.transgui"
      , #flags := [ Gio.ApplicationFlagsFlagsNone ]
      ]
    void (on app #activate (activateApp app))
    void (Gio.applicationRun app Nothing)