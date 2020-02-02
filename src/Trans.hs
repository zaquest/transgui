{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Trans where

import Prelude hiding (init)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import RPC (RPC)
import qualified RPC
import UI (UI)
import qualified UI
import Data.Text (Text)


data Settings = Settings
  { rcpAddress :: String
  }


data Data = Data
  { rpcData :: IORef RPC.Data
  , uiData :: UI.Data
  }


newtype Trans a = Trans { runTrans :: ReaderT Data IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


asks :: (Data -> a) -> Trans a
asks f = Trans (Reader.asks f)

ask :: Trans Data
ask = asks id


rpc :: RPC a -> Trans a
rpc action = do
  moduleData <- asks rpcData
  liftIO $ RPC.run moduleData action


ui :: UI a -> Trans a
ui action = do
  moduleData <- asks uiData
  liftIO $ UI.run moduleData action


init :: Settings -> IO Data
init settings = do
  rpcData <- RPC.init (RPC.Settings (rcpAddress settings))
  uiData <- UI.init
  pure $ Data rpcData uiData


run :: Data -> Trans a -> IO a
run datum (Trans action) = Reader.runReaderT action datum


data Field = Field
  { key :: Text
  }


main :: IO ()
main = do
  datum <- init (Settings "http://192.168.0.100:9091/transmission/rpc")
  run datum $ do
    torrents <- rpc (RPC.torrentGet ["id", "name"])
    liftIO $ print torrents
    ui UI.start
