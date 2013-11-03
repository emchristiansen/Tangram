import Data.Array.Repa.IO.DevIL
import Control.Monad
import Data.Maybe
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Data.Monoid
import Data.Either
import Control.Concurrent.Async
import ImageIO

-- Lazily reads all the images in a directory.
directoryImageProvider :: FilePath -> Producer Image IO ()
directoryImageProvider directory = do
  fileNames <- lift $ imageFilesInDirectory directory
  for (each fileNames) $ \fileName -> do
    maybeImage <- lift $ readImageSafe fileName
    each $ maybeToList maybeImage

imagePool :: Pipe Image Image IO ()
imagePool = do
  image <- await
  yield image

data Tangram = Tangram Image

-- This should be something like Pipe Image Tangram IO ()
tangramMaker :: Pipe Image (Either Image Tangram) IO ()
tangramMaker = do
  image <- await
  yield $ Left image
  --yield $ Right $ Tangram image

--imagePasser :: Pipe Image Image IO ()
--imagePasser = undefined

imageWriter :: Consumer Image IO ()
imageWriter = forever $ do
  image <- await
  lift $ putStrLn "Writing"
  lift $ removeIfExists fileName
  lift $ runIL $ writeImage fileName image
  lift $ threadDelay 2000000  -- Wait 2 seconds
 where fileName = "/home/eric/Downloads/fractal2.png"

takeLeftPipe :: Pipe (Either a b) a IO ()
takeLeftPipe = P.map (\x -> lefts [x]) >-> P.concat

takeRightPipe :: Pipe (Either a b) b IO ()
takeRightPipe = P.map (\x -> rights [x]) >-> P.concat

displaySetter :: Consumer Tangram IO ()
displaySetter = P.map (\(Tangram image) -> image) >-> imageWriter

main :: IO ()
main = do  
  let provider = directoryImageProvider "/home/eric/Bitcasa/data/fractals"

  (imagePoolOutput, imagePoolInput) <- spawn Single
  (tangramFailureOutput, tangramFailureInput) <- spawn Unbounded
  (tangramSuccessOutput, tangramSuccessInput) <- spawn Unbounded

  let fetchImages = provider >-> toOutput imagePoolOutput

  let imageSource = fromInput imagePoolInput
  let tangramSink = toOutput $ tangramFailureOutput <> tangramSuccessOutput
  let makeTangrams = imageSource >-> imagePool >-> tangramMaker >-> tangramSink

  let recycleImages = fromInput tangramFailureInput >-> takeLeftPipe >-> toOutput imagePoolOutput

  let displayTangrams = fromInput tangramSuccessInput >-> takeRightPipe >-> displaySetter

  a <- async $ runEffect fetchImages
  b <- async $ runEffect makeTangrams
  c <- async $ runEffect recycleImages
  d <- async $ runEffect displayTangrams

  mapM_ wait [a, b, c, d]

  --runEffect displayTangrams

  --forkIO $ do runEffect $ provider >-> toOutput imagePoolOutput

  --let foo = imageSource >-> imagePool >-> tangramMaker >-> tangramSink
  --do runEffect $ foo  

  --let foo2 = fromInput tangramImageInput >-> takeLeftPipe >-> toOutput imagePoolOutput
  --runEffect foo2
  
  --let foo3 = fromInput tangramTangramInput >-> takeRightPipe >-> displaySetter

  --let imagePoolRedirect = takeLeftPipe >-> toOutput imagePoolOutput
  --let displaySetterRedirect = takeRightPipe >-> displaySetter
  --let foo4 = imageSource >-> imagePool >-> tangramMaker >-> (imagePoolRedirect <> displaySetterRedirect)

  --provider >-> imagePoolOutput


  --let effect = provider >-> imagePool
  --let maker = tangramMaker >-> splitP
  --let consumer = imageWriter
  --runEffect $ provider >-> consumer
  putStrLn "Done"