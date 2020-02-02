{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveGeneric #-}
module RPC where

import GHC.Generics (Generic)
import Prelude hiding (init)
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
import Data.Aeson (object, (.=), encode, Value(..), ToJSON(..))
import qualified Data.Vector as Vec
import Data.Text (Text)
import Torrent (Torrent)
import qualified Torrent as T
import qualified Response
import qualified Data.Aeson as Aeson


data Settings = Settings
  { rpcAddress :: String
  }


data Data = Data
  { dAddress :: String
  , dManager :: Manager
  , dSessionId :: Maybe BS.ByteString
  }


newtype RPC a = RPC { unRPC :: ReaderT (IORef Data) IO a }
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


requestLBS :: LS.ByteString -> RPC (Response LS.ByteString)
requestLBS body = do
  d <- ask

  req <- liftIO $ parseRequest (dAddress d)
  let req' = req { method = "POST"
                 , requestBody = RequestBodyLBS body }
  let req'' = setSessionId (dSessionId d) req'
  response <- liftIO $ httpLbs req'' (dManager d)
  saveSessionId response

  case statusCode (responseStatus response) of
    409 -> requestLBS body
    _ -> pure response


init :: Settings -> IO (IORef Data)
init settings = do
  manager <- newManager defaultManagerSettings
  let rpcData = Data (rpcAddress settings) manager Nothing
  newIORef rpcData


run :: (IORef Data) -> RPC a -> IO a
run dataRef (RPC action) = Reader.runReaderT action dataRef


asks :: (Data -> a) -> RPC a
asks f = do
  rpcDataRef <- RPC Reader.ask
  liftIO (f <$> readIORef rpcDataRef)


ask :: RPC Data
ask = asks id


torrentGet :: [Text] -> RPC [Torrent]
torrentGet fields = do
  let req = object [ "method" .= ("torrent-get" :: Text)
                   , "arguments" .= object ["fields" .= fields]
                   ]
  resp <- requestLBS (encode req)
  let Right response =
        Aeson.eitherDecode (responseBody resp)
          :: Either String (Response.TorrentGet)
  pure $ Response.torrents $ Response.arguments response


main :: IO ()
main = do
  let settings = RPC.Settings "http://192.168.0.100:9091/transmission/rpc"
  hdr <- RPC.init settings >>= (`RPC.run` RPC.torrentGet ["id", "name"])
  print hdr
