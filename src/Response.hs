{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Response where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), (.:?), (.!=))
import Data.Text (Text)
import Torrent (Torrent, TID)


data Response a = Response
  { result :: Text
  , arguments :: a
  , tag :: Maybe Int
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (Response a)


data TorrentGetArguments = TorrentGetArguments
  { torrents :: [Torrent]
  , removed :: [TID]
  } deriving (Generic, Show)

instance FromJSON TorrentGetArguments where
  parseJSON = withObject "TorrentGetArguments" $ \o ->
    TorrentGetArguments
      <$> o .: "torrents"
      <*> o .:? "removed" .!= []


type TorrentGet = Response TorrentGetArguments
