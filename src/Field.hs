{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Field where

import Prelude hiding (id)
import Data.Proxy (Proxy(..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.GI.Base (GType, IsGValue(..))
import Data.GI.Base.GType (gtypeString, gtypeInt64)
import Some


data Field a = Field
  {
    -- | Transmission torrent structure key
    -- https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
    -- See table in 3.3
    key :: Text
    -- | ListStore index
  , idx :: Int
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


id :: Field Int64
id = Field "id" 0 (Proxy :: Proxy Int64) gtypeInt64


name :: Field Text
name = Field "name" 1 (Proxy :: Proxy Text) gtypeString


allFields :: [Some Field]
allFields = [This id, This name]


total :: Int
total = length allFields


keys :: [Some Field] -> [Text]
keys fs = mapWithSome fs key
