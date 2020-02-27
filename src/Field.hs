{-# LANGUAGE OverloadedStrings, RankNTypes, TypeOperators, InstanceSigs, GADTs #-}
module Field where

import Prelude hiding (id)
import Data.Proxy (Proxy(..))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.GI.Base (GType(gtypeToCGType), IsGValue(..), gtypeName)
import Data.GI.Base.GType (gtypeString, gtypeInt, gtypeInt64, gtypeBoolean)
import Some
import Data.GADT.Compare (GEq(..))
import Data.GADT.Show (GShow(..))
import Data.Type.Equality ((:~:)(..), TestEquality(..))
import GHC.Generics (Generic)
import Type.Reflection
import System.IO.Unsafe (unsafePerformIO)


data Field a = Field
  -- | Transmission torrent structure key
  -- https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
  -- See table in 3.3
  { key :: Text
  -- | ListStore index
  , idx :: Int32
  -- | Haskell type for a field
  , htype :: TypeRep a
  -- | Gtk type for a field
  , gtype :: GType
  } deriving (Eq, Show)


instance IsGValue Text where
  toGValue a = toGValue (Just a)
  fromGValue g = do
    ma <- fromGValue g :: IO (Maybe Text)
    case ma of
      Just a -> pure a
      Nothing -> pure ""


instance Eq GType where
  gt1 == gt2 = gtypeToCGType gt1 == gtypeToCGType gt2


instance Show GType where
  show gt = unsafePerformIO (gtypeName gt)


instance GEq Field where
  geq fa fb = do
    Refl <- testEquality (htype fa) (htype fb)
    if fa == fb
       then Just Refl
       else Nothing


instance GShow Field where
  gshowsPrec = showsPrec


mkField :: Typeable a => Text -> Int32 -> Proxy a -> GType -> Field a
mkField name idx proxy gtype = Field name idx (typeOf (undefined :: a)) gtype


id :: Field Int32
id = mkField "id" 0 (Proxy :: Proxy Int32) gtypeInt


name :: Field Text
name = mkField "name" 1 (Proxy :: Proxy Text) gtypeString


addedDate :: Field Int32
addedDate = mkField "addedDate" 2 (Proxy :: Proxy Int32) gtypeInt


peersGettingFromUs :: Field Int32
peersGettingFromUs = mkField "peersGettingFromUs" 3 (Proxy :: Proxy Int32) gtypeInt


peersSendingToUs :: Field Int32
peersSendingToUs = mkField "peersSendingToUs" 4 (Proxy :: Proxy Int32) gtypeInt


downloadedEver :: Field Int64
downloadedEver = mkField "downloadedEver" 5 (Proxy :: Proxy Int64) gtypeInt64


uploadedEver :: Field Int64
uploadedEver = mkField "uploadedEver" 6 (Proxy :: Proxy Int64) gtypeInt64


isFinished :: Field Bool
isFinished = mkField "isFinished" 7 (Proxy :: Proxy Bool) gtypeBoolean


rateUpload :: Field Int32
rateUpload = mkField "rateUpload" 8 (Proxy :: Proxy Int32) gtypeInt


rateDownload :: Field Int32
rateDownload = mkField "rateDownload" 9 (Proxy :: Proxy Int32) gtypeInt

sizeWhenDone :: Field Int64
sizeWhenDone = mkField "sizeWhenDone" 10 (Proxy :: Proxy Int64) gtypeInt64


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


total :: Integral a => a
total = fromIntegral (length allFields)


keys :: [Some Field] -> [Text]
keys fs = mapWithSome fs key
