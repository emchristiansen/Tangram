module Tangram where

import Tree
import ImageRGBA8

-- We're restricting ourselves to tangrams which can be constructed by
-- either stacking tangrams vertically or putting them side-by-side.
type Tangram = Tree ImageRGBA8
