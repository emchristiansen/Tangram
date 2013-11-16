module RolloutTangramMaker where

import System.Random (StdGen)
import Control.Monad.Random
import Control.Monad
import Control.Monad.Memo
import Control.Monad.Loops
import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map as M
import Pipes
import Pipes.Concurrent

import System
import ImageUtil
import TangramMakerUtil

die :: (RandomGen g) => Rand g Int
die = getRandomR (1,6)

-- A monadic version of scanl.
-- Probably not the most efficient implementation.
scanlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
scanlM function start values = foldM function' [start] values
 where 
  -- A modified version of `function` which passes along the yielded values.
  function' as nextB = do
  	nextA <- function (last as) nextB
  	return $ as ++ [nextA]

addToTangram :: (RandomGen g) => Tangram -> ImageRGBA8 -> Rand g Tangram
addToTangram tangram image = do
  switch <- getRandom
  return $ if switch then Vertical tangram (Leaf image)
  	else Horizontal tangram (Leaf image)

scanlTangrams :: (RandomGen g) => [ImageRGBA8] -> Rand g [Tangram]
scanlTangrams [] = return []
scanlTangrams (image : images) = scanlM addToTangram (Leaf image) images

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle values = do
  index <- getRandomR (0, length permutations' - 1)
  return $ permutations' !! index
 where
  permutations' = permutations values

-- The head of the returned list is the new tangram.
randomlyPairTangrams :: RandomGen g => [Tangram] -> Rand g [Tangram]
randomlyPairTangrams [] = return []
randomlyPairTangrams [_] = return []
randomlyPairTangrams tangrams = do
  leftTangram : rightTangram : otherTangrams <- shuffle tangrams
  switch <- getRandom
  let newTangram = if switch then Vertical leftTangram rightTangram
  	else Horizontal leftTangram rightTangram
  return $ newTangram : otherTangrams

tangramsFromPairing :: RandomGen g => [ImageRGBA8] -> Rand g [Tangram]
tangramsFromPairing [] = return []
tangramsFromPairing [_] = return []
tangramsFromPairing images = do
  --foo <- return leafs >>= randomlyPairTangrams >>= randomlyPairTangrams
  tangrams <- sequence $ iterate (>>= randomlyPairTangrams) $ return leafs
  --newTangrams <- randomlyPairTangrams leafs
  return $ map head $ take numPairs $ tail tangrams
 where
  leafs = map Leaf images
  numPairs = length images - 1


maxImagesInTangram :: Int
maxImagesInTangram = 8

maxPermutations :: Int
maxPermutations = 32

numPairingRuns :: Int
numPairingRuns = 32

--numAttemptsPerPermutation :: Int
--numAttemptsPerPermutation = 32

containsWallpaperSize :: TangramSizes -> Bool
containsWallpaperSize sizes = definedForWidth && heightInRange
 where
  ImageSize width height = wallpaperSize
  fromWidth = fst sizes
  definedForWidth = width `M.member` fromWidth
  (minHeight, maxHeight) = fromWidth M.! width 
  heightInRange = height >= minHeight && height <= maxHeight

firstLegalTangram :: MonadMemo Tangram TangramSizes m => [Tangram] -> m (Maybe Tangram)
firstLegalTangram tangrams = do
  sizes <- mapM (memo legalTangramSizes) tangrams
  return $ (liftM fst) $ find (containsWallpaperSize . snd) $ zip tangrams sizes

imagesToTangram :: (RandomGen g) => [ImageRGBA8] -> Rand g (Maybe Tangram)
imagesToTangram images = do
  pairings <- tangramsFromPairing images
  let tangrams = (map Leaf images) ++ pairings
  return $ startEvalMemo $ firstLegalTangram $ tangrams

--imagesToTangram :: (RandomGen g) => [ImageRGBA8] -> Rand g (Maybe Tangram)
--imagesToTangram images = do
--  let permuted = take maxPermutations $ permutations images
--  tangramsUnflat <- mapM scanlTangrams permuted
--  return $ startEvalMemo $ firstLegalTangram $ concat tangramsUnflat

rolloutTangramMaker :: TangramMaker
rolloutTangramMaker = forever $ do
  images <- replicateM maxImagesInTangram await
  legalTangramMaybe <- lift $ evalRandIO $ imagesToTangram images
  case legalTangramMaybe of 
  	Nothing -> 
  	  -- We failed, so send all the images back.
  	  mapM_ yield $ map Left images
  	Just legalTangram -> do
  	  -- Send back all the images we didn't use.
  	  mapM_ yield $ map Left $ filter (notUsedIn legalTangram) images
  	  -- Send forward our tangram.
  	  yield $ Right legalTangram
 where
   notUsedIn tangram image = not $ image `elem` componentImages tangram
