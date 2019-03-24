{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module RPC where

import Data.List (find)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef)
import Network.HTTP.Client
import Network.HTTP.Types
import Data.ByteString (ByteString)


data RPCSettings = RPCSettings
  { rpcAddress :: String
  }


data RPCData = RPCData
  { dAddress :: String
  , dManager :: Manager
  , dSessionId :: Maybe ByteString
  }


newtype RPC a = RPC { unRPC :: ReaderT (IORef RPCData) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


runRPC :: RPCSettings -> RPC a -> IO a
runRPC settings action = do
  manager <- newManager defaultManagerSettings
  let rpcData = RPCData (rpcAddress settings) manager Nothing
  rpcDataRef <- newIORef rpcData
  Reader.runReaderT (unRPC action) rpcDataRef


asks :: (RPCData -> a) -> RPC a
asks f = do
  rpcDataRef <- RPC Reader.ask
  liftIO (f <$> readIORef rpcDataRef)


ask :: RPC RPCData
ask = asks id


getHeader :: Response body -> HeaderName -> Maybe ByteString
getHeader resp hdr = snd <$> find ((hdr ==) . fst) (responseHeaders resp)


doshit :: RPC (Maybe ByteString)
doshit = do
  manager <- asks dManager
  addr <- asks dAddress

  request <- liftIO $ parseRequest addr
  response <- liftIO $ httpLbs request manager

  pure (getHeader response "X-Transmission-Session-Id")
