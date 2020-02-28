{-# LANGUAGE OverloadedStrings, RankNTypes, TypeOperators, InstanceSigs, GADTs #-}
module Field where

import Prelude hiding (id)
import qualified Data.List as List
import Data.Proxy (Proxy(..))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.GI.Base (GType(gtypeToCGType), IsGValue(..), gtypeName, GValue)
import Data.GI.Base.GType (gtypeString, gtypeInt, gtypeInt64, gtypeBoolean)
import Some
import Data.GADT.Compare (GEq(..))
import Data.GADT.Show (GShow(..))
import Data.Type.Equality ((:~:)(..), TestEquality(..))
import GHC.Generics (Generic)
import Type.Reflection
import System.IO.Unsafe (unsafePerformIO)
import Torrent (Torrent)
import qualified Torrent as T
import Lens.Micro (Lens', (^.))


data Field a = Field
  -- | Transmission torrent structure key
  -- https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
  -- See table in 3.3
  { key :: Text
  -- | ListStore index
  , idx :: Int32
  -- | Haskell type for a field
  , htype :: TypeRep a
  , lens :: Lens' Torrent a
  -- | Gtk type for a field
  , gtype :: GType
  , toGVal :: a -> IO GValue
  , fromGVal :: GValue -> IO a
  }


instance IsGValue Text where
  toGValue a = toGValue (Just a)
  fromGValue g = do
    ma <- fromGValue g :: IO (Maybe Text)
    case ma of
      Just a -> pure a
      Nothing -> pure ""


instance Show (Field a) where
  show f = "Field \"" ++ T.unpack (key f) ++ "\" " ++ show (gtype f)


instance Eq (Field a) where
  f1 == f2 = idx f1 == idx f2


instance Eq GType where
  gt1 == gt2 = gtypeToCGType gt1 == gtypeToCGType gt2


instance Show GType where
  show gt = unsafePerformIO (gtypeName gt)


instance GEq Field where
  geq fa fb = do
    Refl <- testEquality (htype fa) (htype fb)
    if idx fa == idx fb
       then Just Refl
       else Nothing


instance GShow Field where
  gshowsPrec = showsPrec


mkField :: (IsGValue a, Typeable a)
        => Text
        -> Int32
        -> Lens' Torrent a
        -> GType
        -> Field a
mkField name idx lens gtype = Field
  { key = name
  , idx = idx
  , htype = (typeOf (undefined :: a))
  , lens = lens
  , gtype = gtype
  , toGVal = toGValue
  , fromGVal = fromGValue
  }


id :: Field Int32
id = mkField "id" 0 T.id gtypeInt


name :: Field Text
name = mkField "name" 1 T.name gtypeString


addedDate :: Field Int32
addedDate = mkField "addedDate" 2 T.addedDate gtypeInt


peersGettingFromUs :: Field Int32
peersGettingFromUs = mkField "peersGettingFromUs" 3 T.peersGettingFromUs gtypeInt


peersSendingToUs :: Field Int32
peersSendingToUs = mkField "peersSendingToUs" 4 T.peersSendingToUs gtypeInt


downloadedEver :: Field Int64
downloadedEver = mkField "downloadedEver" 5 T.downloadedEver gtypeInt64


uploadedEver :: Field Int64
uploadedEver = mkField "uploadedEver" 6 T.uploadedEver gtypeInt64


isFinished :: Field Bool
isFinished = mkField "isFinished" 7 T.isFinished gtypeBoolean


rateUpload :: Field Int32
rateUpload = mkField "rateUpload" 8 T.rateUpload gtypeInt


rateDownload :: Field Int32
rateDownload = mkField "rateDownload" 9 T.rateDownload gtypeInt

sizeWhenDone :: Field Int64
sizeWhenDone = mkField "sizeWhenDone" 10 T.sizeWhenDone gtypeInt64


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


indices :: [Some Field] -> [Int32]
indices fs = mapWithSome fs idx


getGValue :: Torrent -> Field a -> IO GValue
getGValue t f = (toGVal f) (t ^. (lens f))


gvalue :: Torrent -> Some Field -> IO GValue
gvalue t sf = withSome sf (getGValue t)


gvalues :: [Some Field] -> Torrent -> IO [GValue]
gvalues fs t = traverse (gvalue t) fs


newtype SameField = SF { unSF :: Some Field }


instance Eq SameField where
  (SF sf1) == (SF sf2) = (withSome sf1 idx) == (withSome sf2 idx)


nub :: [Some Field] -> [Some Field]
nub = map unSF . List.nub . map SF
