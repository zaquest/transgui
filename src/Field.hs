{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Field where

import Prelude hiding (id)
import Data.Proxy (Proxy(..))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.GI.Base (GType, IsGValue(..))
import Data.GI.Base.GType (gtypeString, gtypeInt, gtypeInt64, gtypeBoolean)
import Some


data Field a = Field
  -- | Transmission torrent structure key
  -- https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
  -- See table in 3.3
  { key :: Text
  -- | ListStore index
  , idx :: Int32
  -- | Haskell type for a field
  , htype :: Proxy a
  -- | Gtk type for a field
  , gtype :: GType
  }


instance IsGValue Text where
  toGValue a = toGValue (Just a)
  fromGValue g = do
    ma <- fromGValue g :: IO (Maybe Text)
    case ma of
      Just a -> pure a
      Nothing -> pure ""


id :: Field Int32
id = Field "id" 0 (Proxy :: Proxy Int32) gtypeInt


name :: Field Text
name = Field "name" 1 (Proxy :: Proxy Text) gtypeString


addedDate :: Field Int32
addedDate = Field "addedDate" 2 (Proxy :: Proxy Int32) gtypeInt


peersGettingFromUs :: Field Int32
peersGettingFromUs = Field "peersGettingFromUs" 3 (Proxy :: Proxy Int32) gtypeInt


peersSendingToUs :: Field Int32
peersSendingToUs = Field "peersSendingToUs" 4 (Proxy :: Proxy Int32) gtypeInt


downloadedEver :: Field Int64
downloadedEver = Field "downloadedEver" 5 (Proxy :: Proxy Int64) gtypeInt64


uploadedEver :: Field Int64
uploadedEver = Field "uploadedEver" 6 (Proxy :: Proxy Int64) gtypeInt64


isFinished :: Field Bool
isFinished = Field "isFinished" 7 (Proxy :: Proxy Bool) gtypeBoolean


rateUpload :: Field Int32
rateUpload = Field "rateUpload" 8 (Proxy :: Proxy Int32) gtypeInt


rateDownload :: Field Int32
rateDownload = Field "rateDownload" 9 (Proxy :: Proxy Int32) gtypeInt

sizeWhenDone :: Field Int64
sizeWhenDone = Field "sizeWhenDone" 10 (Proxy :: Proxy Int64) gtypeInt64


allFields :: [Some Field]
allFields =
  [ This id
  , This name
  , This addedDate
  , This peersGettingFromUs
  , This peersSendingToUs
  , This downloadedEver
  , This uploadedEver
  , This isFinished
  , This rateUpload
  , This rateDownload
  , This sizeWhenDone
  ]


total :: Int
total = length allFields


keys :: [Some Field] -> [Text]
keys fs = mapWithSome fs key
