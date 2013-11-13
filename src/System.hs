{-# OPTIONS_GHC -fno-warn-orphans #-}

module System where

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

import ImageUtil
import PipeUtil
import Util

-- We're restricting ourselves to tangrams which can be constructed by
-- either stacking tangrams vertically or putting them side-by-side.
data Tangram = 
  Vertical Tangram Tangram | 
  Horizontal Tangram Tangram |
  Leaf ImageRGBA8 deriving (Show, Eq, Ord)

type ImageProducer = Producer ImageRGBA8 IO ()

type ImagePool = Pipe ImageRGBA8 ImageRGBA8 IO ()

type TangramMaker = Pipe ImageRGBA8 (Either ImageRGBA8 Tangram) IO ()

type DisplaySetter = Consumer Tangram IO ()

-- The larger dimension of a resized component image must be at 
-- least this many pixels.
-- We don't want tiny images.
minLargerDimension :: Int
minLargerDimension = 300
-- The smaller dimension of a resized component image may not be more 
-- than this many pixels.
-- We don't want one image taking up the whole tangram.
maxSmallerDimension :: Int
maxSmallerDimension = 600
--minRelativeSize = 0.5
-- We may not change the aspect ratio of an image by more than this constant.
-- We want to keep image cropping to an absolute minimum.
maxAspectWarp :: Double
maxAspectWarp = 1.02
-- Half the width of the pixel border between images.
halfBorderWidth :: Int
halfBorderWidth = 1

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

-- If there are any rejected images, yield a rejected image with 50%
-- probability and an unknown image with 50% probability.
-- Otherwise yield an unknown image.
mixingImagePool :: [ImageRGBA8] -> [ImageRGBA8] -> [ImageRGBA8] -> ImagePool
mixingImagePool freshImages rejectedImages knownImages = do
  lift $ printf "length freshImages: %d\n" $ length freshImages
  lift $ printf "length rejectedImages: %d\n" $ length rejectedImages
  lift $ printf "length knownImages: %d\n" $ length knownImages
  case null freshImages of
    -- Make sure we have at least one fresh image.
    True -> do
      image <- await
      case image `elem` knownImages of
        True -> mixingImagePool freshImages (image : rejectedImages) knownImages
        False -> mixingImagePool (image : freshImages) rejectedImages (image :knownImages)
    False -> case rejectedImages of
      -- If we haven't accumulated any rejects, we yield a fresh image.
      [] -> do
        let freshHead : freshTail = freshImages
        yield freshHead
        mixingImagePool freshTail rejectedImages knownImages
      -- If we have rejects, we flip a coin to determine whether to yield
      -- a reject or a new image.
      rejectedHead : rejectedTail -> do
        yieldRejected <- lift $ randomBool
        case yieldRejected of
          True -> do
            yield rejectedHead
            mixingImagePool freshImages rejectedTail knownImages
          False -> do
            let freshHead : freshTail = freshImages
            yield freshHead
            mixingImagePool freshTail rejectedImages knownImages

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
      lift $ threadDelay 400000

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

  --a <- async $ runEffect fetchImages
  --b <- async $ runEffect makeTangrams
  --c <- async $ runEffect recycleImages
  --d <- async $ runEffect displayTangrams

  --mapM_ wait [a, b, c, d]

  jobsFinished <- mapM (async . runEffect) [
    fetchImages, 
    makeTangrams, 
    recycleImages, 
    displayTangrams]

  mapM_ wait jobsFinished

  return ()