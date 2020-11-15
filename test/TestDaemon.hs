{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

-- | Set up & tear down instance of transmission daemon for
-- integration testings

module TestDaemon where

import Prelude hiding (FilePath)
import Text.RawString.QQ
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Filesystem.Path.CurrentOS as FP
import System.INotify
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Shelly


withTransmission :: (String -> IO a) -> IO a
withTransmission body = shelly . withTmpDir $ \fp ->
  bracket_sh
    (setUp fp)
    (\(_, pid) -> tearDown pid)
    (\(url, _) -> liftIO $ body url)

setUp :: FilePath -> Sh (String, PID)
setUp home = do
  let port = 9091
  writeSettings home port
  pid <- startDaemon home
  pure
    ( "http://localhost:" <> show port <> "/transmission/rpc"
    , pid
    )


tearDown :: PID -> Sh ()
tearDown pid = run_ "kill" [pid]


writeSettings :: FilePath -> Int -> Sh ()
writeSettings home _port =
  writefile (home </> ("settings.json" :: Text)) (settings home 9091)


putWhenCreated :: MVar () -> FilePath -> Event -> IO ()
putWhenCreated wait fp1 (Created _ rfp2) =
  when (FP.filename (FP.decodeString fp1) == FP.filename (FP.decodeString fp2)) (putMVar wait ())
    where fp2 = fromRaw rfp2
putWhenCreated _ _ _ = pure ()


mkWaitFor :: FilePath -> IO (IO ())
mkWaitFor fp = do
  wait <- newEmptyMVar
  inotify <- initINotify
  watch <- addWatch
    inotify
    [Create]
    (BS.pack . FP.encodeString . FP.directory $ FP.decodeString fp)
    (putWhenCreated wait fp)
  pure $ do
    takeMVar wait
    removeWatch watch
    killINotify inotify


runAndWaitFor :: Sh a -> FilePath -> Sh a
runAndWaitFor action fp = do
  wait <- liftIO (mkWaitFor fp)
  result <- action
  liftIO wait
  pure result


type PID = Text


startDaemon :: FilePath -> Sh PID
startDaemon home = do
  runAndWaitFor runDaemon pidFile
  readfile pidFile
    where
      pidFile = home </> ("transmission.pid" :: Text)
      runDaemon = run_ "transmission-daemon"
        [ "--config-dir", encodeText home
        , "--download-dir", encodeText home
        , "--no-incomplete-dir"
        , "--no-watch-dir"
        , "--pid-file", encodeText pidFile
        ]


settings :: FilePath -> Int -> Text
settings home port = [r|{
    "alt-speed-down": 50,
    "alt-speed-enabled": false,
    "alt-speed-time-begin": 540,
    "alt-speed-time-day": 127,
    "alt-speed-time-enabled": false,
    "alt-speed-time-end": 1020,
    "alt-speed-up": 50,
    "bind-address-ipv4": "0.0.0.0",
    "bind-address-ipv6": "::",
    "blocklist-enabled": false,
    "blocklist-url": "http://www.example.com/blocklist",
    "cache-size-mb": 4,
    "dht-enabled": true,
    "download-dir": "|] <> encodeText home <> [r|",
    "download-queue-enabled": true,
    "download-queue-size": 5,
    "encryption": 1,
    "idle-seeding-limit": 30,
    "idle-seeding-limit-enabled": false,
    "incomplete-dir": "|] <> encodeText home <> [r|",
    "incomplete-dir-enabled": false,
    "lpd-enabled": false,
    "message-level": 2,
    "peer-id-ttl-hours": 6,
    "peer-limit-global": 200,
    "peer-limit-per-torrent": 50,
    "peer-port": 51413,
    "peer-port-random-high": 65535,
    "peer-port-random-low": 49152,
    "peer-port-random-on-start": true,
    "peer-socket-tos": "default",
    "pex-enabled": true,
    "port-forwarding-enabled": true,
    "preallocation": 1,
    "prefetch-enabled": true,
    "queue-stalled-enabled": true,
    "queue-stalled-minutes": 30,
    "ratio-limit": 2,
    "ratio-limit-enabled": false,
    "rename-partial-files": true,
    "rpc-authentication-required": false,
    "rpc-bind-address": "0.0.0.0",
    "rpc-enabled": true,
    "rpc-host-whitelist": "",
    "rpc-host-whitelist-enabled": true,
    "rpc-password": "",
    "rpc-port": |] <> T.pack (show port) <> [r|,
    "rpc-url": "/transmission/",
    "rpc-username": "",
    "rpc-whitelist": "127.0.0.1",
    "rpc-whitelist-enabled": true,
    "scrape-paused-torrents-enabled": true,
    "script-torrent-done-enabled": false,
    "script-torrent-done-filename": "",
    "seed-queue-enabled": false,
    "seed-queue-size": 10,
    "speed-limit-down": 100,
    "speed-limit-down-enabled": false,
    "speed-limit-up": 100,
    "speed-limit-up-enabled": false,
    "start-added-torrents": true,
    "trash-original-torrent-files": false,
    "umask": 18,
    "upload-slots-per-torrent": 14,
    "utp-enabled": true
}|]


encodeText :: FilePath -> Text
encodeText fp =
  case FP.toText (FP.decodeString fp) of
    Right t -> t
    Left _ -> error ("Invalid file path" <> show fp)


fromRaw :: ByteString -> FilePath
fromRaw = FP.encodeString . FP.fromText . T.decodeUtf8
