module BruteTangramMaker where

import Control.Monad
import Data.Maybe
import Pipes
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Data.Monoid
import Control.Concurrent.Async
--import Data.List.Utils
import Data.List
import Codec.Picture
import qualified System.Random as R
--import qualified Data.Set as Set
import Text.Printf
import Data.Tuple
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import Control.Applicative
import Control.Monad.Memo
import Data.Function  
import Control.Arrow ((&&&))

import System
import TangramMakerUtil
import Util
import ImageUtil

-- All new consistent tangrams which can be produced by combining pairs of
-- existing tangrams.
pairTangrams :: MonadMemo Tangram TangramSizes m => Constraints -> [Tangram] -> m [Tangram]
pairTangrams constraints components = do
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
  consistent = liftM ((> 0) . numSizes) . memo (legalTangramSizes constraints)

-- All tangrams which can be produced from the given tangrams though at most
-- one pairing.
iterateTangrams :: MonadMemo Tangram TangramSizes m => Constraints -> [Tangram] -> m [Tangram]
iterateTangrams constraints components = 
  liftM2 (++) (return components) (pairTangrams constraints components)

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
allTangrams :: MonadMemo Tangram TangramSizes m => Constraints -> [ImageRGBA8] -> m [Tangram]
allTangrams constraints images = mfix (iterateTangrams constraints) $ return $ map Leaf images