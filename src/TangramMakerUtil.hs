module TangramMakerUtil where

import Control.Monad
{-import Data.Maybe-}
{-import Pipes-}
{-import Pipes.Concurrent-}
{-import Control.Concurrent (threadDelay)-}
{-import Data.Monoid-}
{-import Control.Concurrent.Async-}
--import Data.List.Utils
import Data.List
import Codec.Picture
{-import qualified System.Random as R-}
--import qualified Data.Set as Set
{-import Text.Printf-}
import Data.Tuple
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import Control.Applicative
import Control.Monad.Memo
{-import Data.Function  -}
import Control.Arrow ((&&&))
import Control.Lens

import System
{-import Util-}
import ImageUtil

componentImages :: Tangram -> [ImageRGBA8]
componentImages (Vertical top bottom) = 
  (componentImages top) ++ (componentImages bottom)
componentImages (Horizontal left right) = 
  (componentImages left) ++ (componentImages right)  
componentImages (Leaf image) = [image]	

legalCrops :: Constraints -> Int -> Int -> [(Int, Int)]
legalCrops constraints width height = nub $ sort crops
 where
  aspect width' height' = fromIntegral width' / fromIntegral height'
  trueAspect = aspect width height
  minAspect = trueAspect / constraints ^. maxAspectWarpL
  maxAspect = trueAspect * constraints ^. maxAspectWarpL
  legalAspect width' height' = 
    (aspect width' height' <= maxAspect) && (aspect width' height' >= minAspect)
  legalWidths = filter (\width' -> legalAspect width' height) [0 .. width]
  legalHeights = filter (\height' -> legalAspect width height') [0 .. height]
  crops = 
    ((,) <$> legalWidths <*> [height]) ++ ((,) <$> [width] <*> legalHeights) 

legalRescalingsHelper :: Constraints -> Int -> Int -> [(Int, Int)]
legalRescalingsHelper constraints smallerDimension largerDimension = nub $ sort rescalings
 where
  divide denominator = (/ fromIntegral denominator) . fromIntegral
  smallerScalingFactors :: [Double]
  smallerScalingFactors =
    map (divide smallerDimension) [constraints ^. maxSmallerDimensionL .. smallerDimension]  
  largerScalingFactors :: [Double]
  largerScalingFactors = 
    map (divide largerDimension) [constraints ^. minLargerDimensionL .. largerDimension]
  rescale factor = 
    (round (factor * fromIntegral smallerDimension), round (factor * fromIntegral largerDimension))
  rescalings = map rescale $ smallerScalingFactors ++ largerScalingFactors

legalRescalings :: Constraints -> Int -> Int -> [(Int, Int)]
legalRescalings constraints width height = case width <= height of
  True -> legalRescalingsHelper constraints width height
  False -> map swap $ legalRescalingsHelper constraints height width

legalImageSizes :: Constraints -> Int -> Int -> [(Int, Int)]
legalImageSizes constraints width height = nub $ sort sizes
  where
   crops = legalCrops constraints width height
   sizes = concatMap (uncurry (legalRescalings constraints)) crops

--legalImageSizes :: Int -> Int -> [(Int, Int)]
--legalImageSizes = undefined

type SizeMap = M.Map Int (Int, Int)  

-- A collection of possible sizes of a tangram.
-- For efficiency, it consists of two maps, allowing one to supply a width
-- and get all heights, or supply a height and get all widths.
type TangramSizes = (SizeMap, SizeMap) 
   
mkTangramSizes :: [(Int, Int)] -> TangramSizes
mkTangramSizes sizes = (ranges widthToHeights, ranges heightToWidths)
 where
  widthToHeights = MM.fromList sizes
  heightToWidths = MM.fromList $ map swap sizes
  ranges :: MM.MultiMap Int Int -> M.Map Int (Int, Int)
  ranges = (M.map (minimum &&& maximum)) . MM.toMap

flipSizes :: SizeMap -> SizeMap
flipSizes sizeMap = ranges $ map swap $ concat tuplesSwapped
 where
  fromRange (start, stop) = [start .. stop]
  tuplesSwapped :: [[(Int, Int)]]
  tuplesSwapped = 
    map (\(l, r) -> (,) <$> [l] <*> r) $ M.assocs $ M.map fromRange sizeMap
  ranges = (M.map (minimum &&& maximum)) . MM.toMap . MM.fromList 

numSizes :: TangramSizes -> Int
numSizes tangramSizes = sum counts
 where
  numInRange = (+ 1) . uncurry (-) . swap
  counts = map (numInRange . snd) $ M.assocs $ fst tangramSizes

-- Creates a new map where the keys are the intersection of the keys of
-- the map, and the values are all possible sums of the values from either
-- map.
-- That's probably not super easy to understand.
-- So just think of this as a helper function for `legalTangramSizes`.
addDimensions :: SizeMap -> SizeMap -> SizeMap
addDimensions x y = M.fromList $ map (id &&& values) keys
 where
  keys :: [Int]
  keys = nub $ sort $ intersect (M.keys x) (M.keys y)
  addPairs x' y' = (fst x' + fst y', snd x' + snd y')
  values key = addPairs (x M.! key) (y M.! key)

--c :: Constraints
--c = undefined

----l :: Int -> Int -> [(Int, Int)]
----l = legalImageSizes c

legalTangramSizes :: MonadMemo Tangram TangramSizes m => Constraints -> Tangram -> m TangramSizes

legalTangramSizes constraints (Leaf image) = return $ mkTangramSizes $ map addBorder sizes
 where
  addBorder (width, height) = 
    (width + 2 * constraints ^. halfBorderWidthL, height + 2 * constraints ^. halfBorderWidthL)
  sizes :: [(Int, Int)]
  --sizes = (legalImageSizes constraints) (imageWidth image) (imageHeight image)
  sizes = legalImageSizes constraints (imageWidth image) (imageHeight image)

legalTangramSizes constraints (Vertical top bottom) = do
  topByWidth <- liftM fst $ memo legalTangramSizes' top
  bottomByWidth <- liftM fst $ memo legalTangramSizes' bottom
  let sizesByWidth = addDimensions topByWidth bottomByWidth
  return $ (sizesByWidth, flipSizes sizesByWidth)
 where legalTangramSizes' = legalTangramSizes constraints

legalTangramSizes constraints (Horizontal left right) = do
  leftByHeight <- liftM snd $ memo legalTangramSizes' left
  rightByHeight <- liftM snd $ memo legalTangramSizes' right
  let sizesByHeight = addDimensions leftByHeight rightByHeight
  return $ (flipSizes sizesByHeight, sizesByHeight)  
 where legalTangramSizes' = legalTangramSizes constraints
