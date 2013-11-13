module BruteTangramMaker where

import System
import TangramMakerUtil
import Util

-- All new consistent tangrams which can be produced by combining pairs of
-- existing tangrams.
pairTangrams :: MonadMemo Tangram TangramSizes m => [Tangram] -> m [Tangram]
pairTangrams components = do
  let pairs = filter (uncurry (<)) $ (,) <$> components <*> components
  let combinations = [uncurry Vertical, uncurry Horizontal] <*> pairs :: [Tangram]
  filterM consistent $ filter noDuplicates $ filter novel combinations
 where
  novel :: Tangram -> Bool
  novel tangram = not $ tangram `elem` components
  noDuplicates :: Tangram -> Bool
  noDuplicates tangram = 
    length (componentImages tangram) == 
      length (nub $ sort $ componentImages tangram)
  consistent :: MonadMemo Tangram TangramSizes m => Tangram -> m Bool
  consistent = liftM ((> 0) . numSizes) . memo legalTangramSizes

-- All tangrams which can be produced from the given tangrams though at most
-- one pairing.
iterateTangrams :: MonadMemo Tangram TangramSizes m => [Tangram] -> m [Tangram]
iterateTangrams components = 
  liftM2 (++) (return components) (pairTangrams components)

-- Repeatedly performs a monadic computation until it hits a fixed point,
-- and returns the fixed point.
mfix :: (Eq a, Monad m) => (a -> m a) -> m a -> m a
mfix function monadicInput = do
  input <- monadicInput
  output <- function input
  case input == output of
    True -> return input
    False -> mfix function $ return output

-- All tangrams which can be produced from a given finite list of images.
allTangrams :: MonadMemo Tangram TangramSizes m => [ImageRGBA8] -> m [Tangram]
allTangrams images = mfix iterateTangrams $ return $ map Leaf images