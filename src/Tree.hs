module Tree where

{-import Control.Lens-}

import Axis

-- A two-dimesional binary tree.
-- Joins are along either the vertical or horizontal axis.
-- An example use is to recursively partition a rectangular image plane.
data Tree a = Node {
  _treeAxisL :: Axis,
  _treeTree0L :: Tree a,
  _treeTree1L :: Tree a
} | Leaf {
  _treeElementT :: a
} deriving (Show, Eq, Ord)
-- TODO: Generate fields.
{-makeFields ''Node-}
{-makeFields ''Leaf-}

