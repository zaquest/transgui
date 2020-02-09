{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Torrent where

import Prelude hiding (id)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Aeson (Object, (.:?), withObject)
import Data.Aeson.Types (Parser, FromJSON(..))
import Data.Default (Default(..))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Lens.Micro.TH


data Torrent = Torrent
  { _id :: Int32
  , _name :: Text
  , _peersGettingFromUs :: Int32
  , _peersSendingToUs :: Int32
  } deriving (Show)


makeLenses ''Torrent


get :: (FromJSON a) => Object -> Text -> a -> Parser a
get obj key val = (obj .:? key) <&> (fromMaybe val)


instance FromJSON Torrent where
  parseJSON = withObject "Torrent" $ \v -> Torrent
    <$> get v "id" (_id def)
    <*> get v "name" (_name def)
    <*> get v "peersGettingFromUs" (_peersGettingFromUs def)
    <*> get v "peersSendingToUs" (_peersSendingToUs def)


defaultTorrent :: Torrent
defaultTorrent = Torrent
  { _id = 0
  , _name = ""
  , _peersGettingFromUs = 0
  , _peersSendingToUs = 0
  }


instance Default Torrent where
  def = defaultTorrent
