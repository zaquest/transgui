{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module RPC where

import Data.List (find)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS


data RPCSettings = RPCSettings
  { rpcAddress :: String
  }


data RPCData = RPCData
  { dAddress :: String
  , dManager :: Manager
  , dSessionId :: Maybe BS.ByteString
  }


newtype RPC a = RPC { unRPC :: ReaderT (IORef RPCData) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


hSessionId :: HeaderName
hSessionId = "X-Transmission-Session-Id"


setSessionId :: Maybe BS.ByteString -> Request -> Request
setSessionId Nothing req = req
setSessionId (Just sessionId) req =
  let hdrs = requestHeaders req
   in req { requestHeaders = (hSessionId, sessionId) : hdrs }


getHeader :: Response body -> HeaderName -> Maybe BS.ByteString
getHeader resp hdr = snd <$> find ((hdr ==) . fst) (responseHeaders resp)


saveSessionId :: Response body -> RPC ()
saveSessionId resp = do
  let hdr = getHeader resp hSessionId
  rpcDataRef <- RPC Reader.ask
  liftIO $ modifyIORef rpcDataRef (\d -> d { dSessionId = hdr })


request :: RPC (Response LS.ByteString)
request = do
  d <- ask

  req <- liftIO $ parseRequest (dAddress d)
  let req' = setSessionId (dSessionId d) req
  response <- liftIO $ httpLbs req' (dManager d)
  saveSessionId response

  case statusCode (responseStatus response) of
    409 -> request
    _ -> pure response


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


doshit :: RPC ()
doshit = do
  resp <- request
  liftIO $ putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
  liftIO $ print $ responseBody resp
