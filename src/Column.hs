{-# LANGUAGE OverloadedStrings #-}
module Column where

import Prelude hiding (id)
import Data.Some
import Data.Text (Text)
import Field (Field)
import qualified Field


data Column = Column
  { title :: Text
  , dependencies :: [Some Field]
  }


keys :: Column -> [Text]
keys = Field.keys . dependencies


id :: Column
id = Column "ID" [This Field.id]


name :: Column
name = Column "Name" [This Field.name]


allColumns :: [Column]
allColumns = [id, name]
