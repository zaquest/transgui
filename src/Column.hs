{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Column where

import Prelude hiding (id)
import Data.List (nub)
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
id = Column "ID" [This Field.id] RenderText


name :: Column
name = Column "Name" [This Field.name] RenderText


addedOn :: Column
addedOn = Column "Added on" [This Field.addedDate] RenderText


leachers :: Column
leachers = Column "Leachers" [This Field.peersGettingFromUs] RenderText


seeders :: Column
seeders = Column "Seeders" [This Field.peersSendingToUs] RenderText


downloaded :: Column
downloaded = Column "Downloaded" [This Field.downloadedEver] RenderText


uploaded :: Column
uploaded = Column "Uploaded" [This Field.uploadedEver] RenderText


done :: Column
done = Column "Done" [This Field.isFinished] RenderText


uploadSpeed :: Column
uploadSpeed = Column "Upload speed" [This Field.rateUpload] RenderText


downloadSpeed :: Column
downloadSpeed = Column "Download speed" [This Field.rateDownload] RenderText


size :: Column
size = Column "Size" [This Field.sizeWhenDone] RenderText


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
collectFields cols = nub . concat $ map dependencies cols
