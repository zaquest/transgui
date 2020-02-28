{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Torrent where

import Prelude hiding (id)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Aeson (Object, (.:?), withObject)
import Data.Aeson.Types (Parser, FromJSON(..))
import Data.Default (Default(..))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Lens.Micro.TH


type TID = Int32


data Torrent = Torrent
  { _id :: TID
  , _name :: Text
  , _addedDate :: Int32
  , _peersGettingFromUs :: Int32
  , _peersSendingToUs :: Int32
  , _downloadedEver :: Int64
  , _uploadedEver :: Int64
  , _isFinished :: Bool
  , _rateUpload :: Int32
  , _rateDownload :: Int32
  , _sizeWhenDone :: Int64
  } deriving (Show)


makeLenses ''Torrent


get :: (FromJSON a) => Object -> Text -> a -> Parser a
get obj key val = (obj .:? key) <&> (fromMaybe val)


instance FromJSON Torrent where
  parseJSON = withObject "Torrent" $ \v -> Torrent
    <$> get v "id" (_id def)
    <*> get v "name" (_name def)
    <*> get v "addedDate" (_addedDate def)
    <*> get v "peersGettingFromUs" (_peersGettingFromUs def)
    <*> get v "peersSendingToUs" (_peersSendingToUs def)
    <*> get v "downloadedEver" (_downloadedEver def)
    <*> get v "uploadedEver" (_uploadedEver def)
    <*> get v "isFinished" (_isFinished def)
    <*> get v "rateUpload" (_rateUpload def)
    <*> get v "rateDownload" (_rateDownload def)
    <*> get v "sizeWhenDone" (_sizeWhenDone def)


defaultTorrent :: Torrent
defaultTorrent = Torrent
  { _id = 0
  , _name = ""
  , _addedDate = 0
  , _peersGettingFromUs = 0
  , _peersSendingToUs = 0
  , _downloadedEver = 0
  , _uploadedEver = 0
  , _isFinished = False
  , _rateUpload = 0
  , _rateDownload = 0
  , _sizeWhenDone = 0
  }


instance Default Torrent where
  def = defaultTorrent
