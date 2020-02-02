{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Response where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Torrent (Torrent)


data Response a = Response
  { result :: Text
  , arguments :: a
  , tag :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (Response a)


data TorrentGetArguments = TorrentGetArguments
  { torrents :: [Torrent]
  , removed :: Maybe [Int]
  } deriving (Generic, Show)

instance FromJSON TorrentGetArguments


type TorrentGet = Response TorrentGetArguments
