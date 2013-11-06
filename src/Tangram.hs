--import Data.Array.Repa.IO.DevIL
import Control.Monad
import Data.Maybe
import Pipes
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Data.Monoid
import Control.Concurrent.Async
import Data.List.Utils
import Codec.Picture
import ImageIO
import PipeUtil
import qualified System.Random as R
import qualified Data.Set as Set
import Text.Printf
  
data Tangram = Tangram ImageRGBA8

type ImageProducer = Producer ImageRGBA8 IO ()

type ImagePool = Pipe ImageRGBA8 ImageRGBA8 IO ()

type TangramMaker = Pipe ImageRGBA8 (Either ImageRGBA8 Tangram) IO ()

type DisplaySetter = Consumer Tangram IO ()

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


  --case null rejectedImages of
  --  False -> 
  --if null rejectedImages

  --switch <- randomBool


--debugImagePool :: [ImageRGBA8] -> [(Int, ImageRGBA8)] -> ImagePool
--debugImagePool usedImages unusedImages = do
--  --lift $ putStrLn $ show $ map fst agingImages
--  image <- await
--  let agingImages' = if imageUnknown image then ((0, image) : agingImages) else agingImages
--  let head' @ (numUsages, image') = head agingImages'
--  let agingImages'' = replace [head'] [(numUsages + 1, image')] agingImages'
--  yield image'
--  debugImagePool agingImages''
-- where imageUnknown image = not $ elem image $ map snd agingImages

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
      yield $ Right $ Tangram image
  

debugDisplaySetter :: FilePath -> DisplaySetter
debugDisplaySetter filePath = forever $ do
  Tangram image <- await
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

main :: IO ()
main = do  
  let imageProducer = directoryImageProducer "/home/eric/Bitcasa/data/fractals" []
  --let imagePool = cat
  let imagePool = myImagePool [] [] []
  let tangramMaker = debugTangramMaker
  let displaySetter = debugDisplaySetter "/home/eric/Downloads/tangram.png"

  runSystem imageProducer imagePool tangramMaker displaySetter

  putStrLn "Done"