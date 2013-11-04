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
  
data Tangram = Tangram DynamicImage

type ImageProducer = Producer DynamicImage IO ()

type ImagePool = Pipe DynamicImage DynamicImage IO ()

type TangramMaker = Pipe DynamicImage (Either DynamicImage Tangram) IO ()

type DisplaySetter = Consumer Tangram IO ()

-- Lazily reads all the images in a directory.
directoryImageProducer :: FilePath -> ImageProducer
directoryImageProducer directory = do
  fileNames <- lift $ imageFilesInDirectory directory
  for (each fileNames) $ \fileName -> do
    maybeImage <- lift $ readImageSafe fileName
    each $ maybeToList maybeImage

debugImagePool :: [(Int, DynamicImage)] -> ImagePool
debugImagePool agingImages = do
  image <- await
  let agingImages' = if imageUnknown then ((0, image) : agingImages) else agingImages
  let head' @ (numUsages, image') = head agingImages'
  --let agingImages'' = replace [head'] [(numUsages + 1, image')] agingImages'
  --yield image'
  --debugImagePool agingImages''
  undefined
 where imageUnknown = True


-- This should be something like Pipe Image Tangram IO ()
debugTangramMaker :: TangramMaker
debugTangramMaker = forever $ do
  image <- await
  --yield $ Left image
  yield $ Right $ Tangram image

debugDisplaySetter :: FilePath -> DisplaySetter
debugDisplaySetter filePath = forever $ do
  Tangram image <- await
  lift $ putStrLn "Writing"
  lift $ savePngImage filePath image
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
  let imageProducer = directoryImageProducer "/home/eric/Bitcasa/data/fractals"
  let imagePool = cat
  let tangramMaker = debugTangramMaker
  let displaySetter = debugDisplaySetter "/home/eric/Downloads/tangram.png"

  runSystem imageProducer imagePool tangramMaker displaySetter

  putStrLn "Done"