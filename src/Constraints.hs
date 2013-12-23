module Constraints where

import Control.Lens

import Size

data Constraints = Constraints {
  _constraintsWallpaperSizeL :: Size,
  _constraintsMinLargerDimensionL :: Int,
  _constraintsMaxSmallerDimensionL :: Int,
  _constraintsMaxAspectWarpL :: Double,
  _constraintsHalfBorderWidthL :: Int
}
makeFields ''Constraints
