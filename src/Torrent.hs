{-# LANGUAGE OverloadedStrings #-}
module Torrent where

import Prelude hiding (id)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Aeson (Object, (.:?), withObject)
import Data.Aeson.Types (Parser, FromJSON(..))
import Data.Default (Default(..))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)


data Torrent = Torrent
  { id :: Int32
  , name :: Text
  , peersGettingFromUs :: Int32
  , peersSendingToUs :: Int32
  } deriving (Show)


get :: (FromJSON a) => Object -> Text -> a -> Parser a
get obj key val = (obj .:? key) <&> (fromMaybe val)


instance FromJSON Torrent where
  parseJSON = withObject "Torrent" $ \v -> Torrent
    <$> get v "id" (id def)
    <*> get v "name" (name def)
    <*> get v "peersGettingFromUs" (peersGettingFromUs def)
    <*> get v "peersSendingToUs" (peersSendingToUs def)


defaultTorrent :: Torrent
defaultTorrent = Torrent
  { id = 0
  , name = ""
  , peersGettingFromUs = 0
  , peersSendingToUs = 0
  }


instance Default Torrent where
  def = defaultTorrent
