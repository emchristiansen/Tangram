module Axis where

-- A two-dimensional axis.
-- Used to denote whether two subelements are merged vertically or horizontally
-- to form a larger element.
data Axis = Horizontal | Vertical deriving (Show, Eq, Ord)

