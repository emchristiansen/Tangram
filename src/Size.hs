module Size where

import Control.Lens

data Size = Size {
  _sizeWidthL :: Int,
  _sizeHeightL :: Int
} deriving (Show, Eq, Ord)
makeFields ''Size
