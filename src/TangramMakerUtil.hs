module TangramMakerUtil where

import System
import Util

componentImages :: Tangram -> [ImageRGBA8]
componentImages (Vertical top bottom) = 
  (componentImages top) ++ (componentImages bottom)
componentImages (Horizontal left right) = 
  (componentImages left) ++ (componentImages right)  
componentImages (Leaf image) = [image]	

legalCrops :: Int -> Int -> [(Int, Int)]
legalCrops width height = nub $ sort crops
 where
  aspect width' height' = fromIntegral width' / fromIntegral height'
  trueAspect = aspect width height
  minAspect = trueAspect / maxAspectWarp
  maxAspect = trueAspect * maxAspectWarp
  legalAspect width' height' = 
    (aspect width' height' <= maxAspect) && (aspect width' height' >= minAspect)
  legalWidths = filter (\width' -> legalAspect width' height) [0 .. width]
  legalHeights = filter (\height' -> legalAspect width height') [0 .. height]
  crops = 
    ((,) <$> legalWidths <*> [height]) ++ ((,) <$> [width] <*> legalHeights) 

legalRescalingsHelper :: Int -> Int -> [(Int, Int)]
legalRescalingsHelper smallerDimension largerDimension = nub $ sort rescalings
 where
  divide denominator = (/ fromIntegral denominator) . fromIntegral
  smallerScalingFactors :: [Double]
  smallerScalingFactors =
    map (divide smallerDimension) [maxSmallerDimension .. smallerDimension]  
  largerScalingFactors :: [Double]
  largerScalingFactors = 
    map (divide largerDimension) [minLargerDimension .. largerDimension]
  rescale factor = 
    (round (factor * fromIntegral smallerDimension), round (factor * fromIntegral largerDimension))
  rescalings = map rescale $ smallerScalingFactors ++ largerScalingFactors

legalRescalings :: Int -> Int -> [(Int, Int)]
legalRescalings width height = case width <= height of
  True -> legalRescalingsHelper width height
  False -> map swap $ legalRescalingsHelper height width

legalImageSizes :: Int -> Int -> [(Int, Int)]
legalImageSizes width height = nub $ sort sizes
  where
   crops = legalCrops width height
   sizes = concatMap (uncurry legalRescalings) crops

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
addDimensions left right = M.fromList $ map (id &&& values) keys
 where
  keys :: [Int]
  keys = nub $ sort $ intersect (M.keys left) (M.keys right)
  addPairs left' right' = (fst left' + fst right', snd left' + snd right')
  values key = addPairs (left M.! key) (right M.! key)

legalTangramSizes :: MonadMemo Tangram TangramSizes m => Tangram -> m TangramSizes

legalTangramSizes (Leaf image) = return $ mkTangramSizes $ map addBorder sizes
 where
  addBorder (width, height) = 
    (width + 2 * halfBorderWidth, height + 2 * halfBorderWidth)
  sizes = legalImageSizes (imageWidth image) (imageHeight image)

legalTangramSizes (Vertical top bottom) = do
  topByWidth <- liftM fst $ memo legalTangramSizes top
  bottomByWidth <- liftM fst $ memo legalTangramSizes bottom
  let sizesByWidth = addDimensions topByWidth bottomByWidth
  return $ (sizesByWidth, flipSizes sizesByWidth)

legalTangramSizes (Horizontal left right) = do
  leftByHeight <- liftM snd $ memo legalTangramSizes left
  rightByHeight <- liftM snd $ memo legalTangramSizes right
  let sizesByHeight = addDimensions leftByHeight rightByHeight
  return $ (flipSizes sizesByHeight, sizesByHeight)  