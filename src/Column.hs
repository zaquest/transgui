{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Column where

import Prelude hiding (id)
import Data.Text (Text)
import Data.Some
import Field (Field)
import qualified Field
import Data.GI.Base
import qualified GI.Gtk as Gtk


data Renderer = RenderText
              | RenderFunc
  deriving (Eq, Show)


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
addedOn = Column "Added on" [mkSome Field.addedDate] RenderText


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
    _ -> error "not implemented"


collectFields :: [Column] -> [Some Field]
collectFields cols = Field.nub . concat $ map dependencies cols
