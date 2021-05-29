{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
module Column where

import Prelude hiding (id)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Some
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Field (Field(..))
import qualified Field
import Data.GI.Base
import qualified GI.Gtk as Gtk


data Renderer = RenderText | RenderFunc Gtk.TreeCellDataFunc


data Column = Column
  { title :: Text
  , dependencies :: [Some Field]
  , renderer :: Renderer
  }


keys :: Column -> [Text]
keys = Field.keys . dependencies


id :: Column
id = Column "ID" [mkSome Field.id] RenderText


name :: Column
name = Column "Name" [mkSome Field.name] RenderText


addedOn :: Column
addedOn = Column "Added on" [mkSome Field.addedDate] (RenderFunc $ renderAddedOn Field.addedDate)


leachers :: Column
leachers = Column "Leachers" [mkSome Field.peersGettingFromUs] RenderText


seeders :: Column
seeders = Column "Seeders" [mkSome Field.peersSendingToUs] RenderText


downloaded :: Column
downloaded = Column "Downloaded" [mkSome Field.downloadedEver] RenderText


uploaded :: Column
uploaded = Column "Uploaded" [mkSome Field.uploadedEver] RenderText


done :: Column
done = Column "Done" [mkSome Field.isFinished] RenderText


uploadSpeed :: Column
uploadSpeed = Column "Upload speed" [mkSome Field.rateUpload] RenderText


downloadSpeed :: Column
downloadSpeed = Column "Download speed" [mkSome Field.rateDownload] RenderText


size :: Column
size = Column "Size" [mkSome Field.sizeWhenDone] RenderText


allColumns :: [Column]
allColumns =
  [ id
  , name
  , addedOn
  , leachers
  , seeders
  , downloaded
  , uploaded
  , done
  , uploadSpeed
  , downloadSpeed
  , size
  ]


mkTreeViewColumn :: Gtk.TreeView -> Column -> IO Gtk.TreeViewColumn
mkTreeViewColumn tv (Column title (field:_) renderer) = do
  col <- new Gtk.TreeViewColumn [ #title := title ]
  case renderer of
    RenderText -> do
      cellRenderer <- new Gtk.CellRendererText []
      #packStart col cellRenderer True
      #appendColumn tv col
      let idx = withSome field Field.idx
      #addAttribute col cellRenderer "text" idx
      pure col
    RenderFunc func -> do
      cellRenderer <- new Gtk.CellRendererText []
      #packStart col cellRenderer True
      #appendColumn tv col
      #setCellDataFunc col cellRenderer (Just func)
      pure col
    -- _ -> error "not implemented"


collectFields :: [Column] -> [Some Field]
collectFields = Field.nub . concatMap dependencies


renderAddedOn :: Field Int32
              -> Gtk.TreeViewColumn
              -> Gtk.CellRenderer
              -> Gtk.TreeModel
              -> Gtk.TreeIter
              -> IO ()
renderAddedOn Field{idx, fromGVal} col renderer model iter = do
  timestamp <- fromGVal =<< #getValue model iter idx
  time <- formatTimestamp timestamp
  rendererText <- fromJust <$> castTo Gtk.CellRendererText renderer
  set rendererText [ #text := time ]


formatTimestamp :: Int32 -> IO Text
formatTimestamp timestamp = do
  let utcTime = posixSecondsToUTCTime (realToFrac timestamp)
  tz <- getCurrentTimeZone
  let time = utcToLocalTime tz utcTime
  let timeString = formatTime defaultTimeLocale "%H:%M:%S %d.%m.%Y" time
  pure (pack timeString)
