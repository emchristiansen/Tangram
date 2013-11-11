module TangramMisc where

import Control.Monad
import Data.Maybe
import Pipes
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Data.Monoid
import Control.Concurrent.Async
import Data.List.Utils
import Data.List
import Codec.Picture
import ImageIO
import PipeUtil
import qualified System.Random as R
import qualified Data.Set as Set
import Text.Printf
import Data.Tuple
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import Control.Applicative
  
type ImageProducer = Producer ImageRGBA8 IO ()

type ImagePool = Pipe ImageRGBA8 ImageRGBA8 IO ()

type TangramMaker = Pipe ImageRGBA8 (Either ImageRGBA8 Tangram) IO ()

type DisplaySetter = Consumer Tangram IO ()

-- The larger dimension of a resized component image must be at 
-- least this many pixels.
-- We don't want tiny images.
minLargerDimension = 300 :: Int
-- The smaller dimension of a resized component image may not be more 
-- than this many pixels.
-- We don't want one image taking up the whole tangram.
maxSmallerDimension = 600 :: Int
--minRelativeSize = 0.5
-- We may not change the aspect ratio of an image by more than this constant.
-- We want to keep image cropping to an absolute minimum.
maxAspectWarp = 1.05 :: Double
-- Half the width of the pixel border between images.
halfBorderWidth = 1 :: Int

-- We're restricting ourselves to tangrams which can be constructed by
-- either stacking tangrams vertically or putting them side-by-side.
data Tangram = 
  Vertical Tangram Tangram | 
  Horizontal Tangram Tangram |
  Leaf ImageRGBA8

legalCrops :: Int -> Int -> [(Int, Int)]
legalCrops width height = nub $ sort crops
 where
  aspect width' height' = fromIntegral width' / fromIntegral height'
  trueAspect = aspect width height
  minAspect = trueAspect / maxAspectWarp
  maxAspect = trueAspect * maxAspectWarp
  legalAspect width height = 
    (aspect width height <= maxAspect) && (aspect width height >= minAspect)
  legalWidths = filter (\width' -> legalAspect width' height) [0 .. width]
  legalHeights = filter (\height' -> legalAspect width height') [0 .. height]
  crops = (zip legalWidths (repeat height)) ++ (zip (repeat width) legalHeights) 

legalRescalingsHelper :: Int -> Int -> [(Int, Int)]
legalRescalingsHelper smallerDimension largerDimension = nub $ sort rescalings
 where
  divide denominator = (/ fromIntegral denominator) . fromIntegral
  smallerScalingFactors =
    map (divide smallerDimension) [maxSmallerDimension .. smallerDimension]  
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

type SizeMap = MM.MultiMap Int Int

-- A collection of possible sizes of a tangram.
-- For efficiency, it consists of two maps, allowing one to supply a width
-- and get all heights, or supply a height and get all widths.
type TangramSizes = (SizeMap, SizeMap)

mkTangramSizes :: [(Int, Int)] -> TangramSizes
mkTangramSizes sizes = (widthToHeightsMap, heightToWidthsMap)
 where
  widthToHeightsMap = MM.fromList sizes
  heightToWidthsMap = MM.fromList $ map swap sizes

-- Creates a new map where the keys are the intersection of the keys of
-- the map, and the values are all possible sums of the values from either
-- map.
-- That's probably not super easy to understand.
-- So just think of this as a helper function for `legalTangramSizes`.
addDimensions :: MM.MultiMap Int Int -> MM.MultiMap Int Int -> [(Int, Int)]
addDimensions left right = concatMap keyValueTuples keys
 where
  keys = nub $ sort $ intersect (MM.keys left) (MM.keys right)
  values key = nub $ sort $ (+) <$> left MM.! key <*> right MM.! key
  keyValueTuples key = (,) <$> [key] <*> values key

legalTangramSizes :: Tangram -> TangramSizes

legalTangramSizes (Leaf image) = mkTangramSizes $ map addBorder sizes
 where
  addBorder (width, height) = 
    (width + 2 * halfBorderWidth, height + 2 * halfBorderWidth)
  sizes = legalImageSizes (imageWidth image) (imageHeight image)

legalTangramSizes (Vertical top bottom) = 
  mkTangramSizes $ addDimensions topWidthToHeightsMap bottomWidthToHeightsMap
 where
  topWidthToHeightsMap = fst $ legalTangramSizes top
  bottomWidthToHeightsMap = fst $ legalTangramSizes bottom

legalTangramSizes (Horizontal left right) = 
  mkTangramSizes $ map swap $ addDimensions leftHeightToWidthsMap rightHeightToWidthsMap
 where
  leftHeightToWidthsMap = snd $ legalTangramSizes left
  rightHeightToWidthsMap = snd $ legalTangramSizes right

-- Lazily reads all the images in a directory.
-- It even selects the next path lazily (though not super efficiently), so
-- the user can drop in new files while the program is running and have them
-- picked up by this producer.
directoryImageProducer :: FilePath -> [FilePath] -> ImageProducer
directoryImageProducer directory usedFiles = do
  allPaths <- lift $ imageFilesInDirectory directory
  case listToMaybe (fresh allPaths) of 
    Nothing -> return ()
    Just freshFile -> do
      maybeImage <- lift $ readImageSafe freshFile
      case maybeImage of 
        Nothing -> recurse $ freshFile : usedFiles
        Just image -> do
          yield image
          recurse $ freshFile : usedFiles
 where 
  fresh = filter (not . (`elem` usedFiles))
  recurse = directoryImageProducer directory

randomBool :: IO Bool
randomBool = do
  random <- R.newStdGen
  return $ even $ fst $ R.next random

-- If there are any rejected images, yield a rejected image with 50%
-- probability and an unknown image with 50% probability.
-- Otherwise yield an unknown image.
myImagePool :: [ImageRGBA8] -> [ImageRGBA8] -> [ImageRGBA8] -> ImagePool
myImagePool freshImages rejectedImages knownImages = do
  lift $ printf "length freshImages: %d\n" $ length freshImages
  lift $ printf "length rejectedImages: %d\n" $ length rejectedImages
  lift $ printf "length knownImages: %d\n" $ length knownImages
  case null freshImages of
    -- Make sure we have at least one fresh image.
    True -> do
      image <- await
      case image `elem` knownImages of
        True -> myImagePool freshImages (image : rejectedImages) knownImages
        False -> myImagePool (image : freshImages) rejectedImages (image :knownImages)
    False -> case rejectedImages of
      -- If we haven't accumulated any rejects, we yield a fresh image.
      [] -> do
        let freshHead : freshTail = freshImages
        yield freshHead
        myImagePool freshTail rejectedImages knownImages
      -- If we have rejects, we flip a coin to determine whether to yield
      -- a reject or a new image.
      rejectedHead : rejectedTail -> do
        yieldRejected <- lift $ randomBool
        case yieldRejected of
          True -> do
            yield rejectedHead
            myImagePool freshImages rejectedTail knownImages
          False -> do
            let freshHead : freshTail = freshImages
            yield freshHead
            myImagePool freshTail rejectedImages knownImages

-- This should be something like Pipe Image Tangram IO ()
debugTangramMaker :: TangramMaker
debugTangramMaker = forever $ do
  image <- await
  switch <- lift $ randomBool
  case switch of
    False -> do
      lift $ putStrLn "Simulating failure"
      yield $ Left image
    True -> do
      lift $ putStrLn "Simulating success"
      yield $ Right $ Leaf image
  

debugDisplaySetter :: FilePath -> DisplaySetter
debugDisplaySetter filePath = forever $ do
  tangram <- await
  case tangram of
    Leaf image -> do
      lift $ putStrLn "Writing"
      lift $ writePng filePath image
      lift $ threadDelay 400000  -- Wait 2 seconds

runSystem :: ImageProducer -> ImagePool -> TangramMaker -> DisplaySetter -> IO ()
runSystem imageProducer imagePool tangramMaker displaySetter = do
  (imagePoolOutput, imagePoolInput) <- spawn Single
  (tangramFailureOutput, tangramFailureInput) <- spawn Unbounded
  (tangramSuccessOutput, tangramSuccessInput) <- spawn Single

  let fetchImages = imageProducer >-> toOutput imagePoolOutput

  let input = fromInput imagePoolInput
  let output = toOutput $ tangramFailureOutput <> tangramSuccessOutput
  let makeTangrams = input >-> imagePool >-> tangramMaker >-> output

  let recycleImages = fromInput tangramFailureInput >-> takeLeftPipe >-> toOutput imagePoolOutput

  let displayTangrams = fromInput tangramSuccessInput >-> takeRightPipe >-> displaySetter

  a <- async $ runEffect fetchImages
  b <- async $ runEffect makeTangrams
  c <- async $ runEffect recycleImages
  d <- async $ runEffect displayTangrams

  mapM_ wait [a, b, c, d]

  return ()