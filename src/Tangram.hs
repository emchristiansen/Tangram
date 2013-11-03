import Data.Array.Repa.IO.DevIL
import System.Directory
import System.FilePath.Posix
import Data.Char
import Control.Exception
import Control.Monad
import Data.Maybe
import Pipes
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import System.IO.Error

-- https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- Just moves an image between two known folders.
moveImage :: IL ()
moveImage = do
  image <- readImage "/home/eric/Bitcasa/data/fractals/murayama_14.jpg"
  writeImage "/home/eric/Downloads/fractal.png" image

-- Checks if a filename is an image filename for some standard extensions.
isImageFilename :: FilePath -> Bool
isImageFilename filename = extension `elem` [".bmp", ".jpeg", ".jpg", ".png"]
 where extension = map toLower $ takeExtension filename

imageFilesInDirectory :: FilePath -> IO [FilePath]
imageFilesInDirectory directory = do
  allFilenames <- getDirectoryContents directory
  return $ map ((directory ++ "/") ++) $ filter isImageFilename allFilenames

readImageSafe :: FilePath -> IO (Maybe Image)
readImageSafe filename = do
  putStrLn $ "Reading " ++ filename
  catchAny (fmap Just (runIL (readImage filename))) $ \exception -> do
  	putStrLn $ "Could not read image, skipping: " ++ filename
  	return Nothing  

maybeReadImages :: [FilePath] -> ListT IO (Maybe Image)
maybeReadImages fileNames = do
    fileName <- Select $ each fileNames
    liftIO $ readImageSafe fileName

flattenImageStream :: (Monad m) => ListT m (Maybe a) -> ListT m a
flattenImageStream stream = do
    ma <- stream
    case ma of
        Just a  -> return a
        Nothing -> mzero

readImages :: [FilePath] -> ListT IO Image
readImages fileNames = flattenImageStream $ maybeReadImages fileNames

-- Lazily reads all the images in a directory.
-- This fairly strange type is necessary to marry laziness and the possibility
-- an image won't load.
-- See this thread:
-- http://stackoverflow.com/questions/19735245/haskell-hiding-failures-in-lazy-io/19736305#19736305
readImagesFromDirectory :: FilePath -> IO (ListT IO Image)
readImagesFromDirectory directory = do
  fileNames <- imageFilesInDirectory directory
  return $ readImages fileNames

directoryImageProvider :: FilePath -> Producer Image IO ()
directoryImageProvider directory = do
  fileNames <- lift $ imageFilesInDirectory directory
  for (each fileNames) $ \fileName -> do
    maybeImage <- lift $ readImageSafe fileName
    each $ maybeToList maybeImage

imageWriter :: Consumer Image IO ()
imageWriter = do
  lift $ putStrLn "Writing"
  image <- await
  lift $ removeIfExists fileName
  lift $ runIL $ writeImage fileName image
  lift $ threadDelay 200000  -- Wait 2 seconds
  imageWriter
 where fileName = "/home/eric/Downloads/fractal2.png"

--acidRain = forever $ do
--    lift $ threadDelay 2000000  -- Wait 2 seconds
--    yield (Harm 1)

--readImagesFromDirectory :: FilePath -> IO [IO (Maybe Image)]
--readImagesFromDirectory directory = do
--  filenames <- imageFilesInDirectory directory
--  return $ map readImageSafe filenames

--filterImages :: IO [IO (Maybe Image)] -> IO [IO [Image]]
--filterImages imageClosures = fmap (fmap (fmap maybeToList)) imageClosures

main :: IO ()
main = do
  let provider = directoryImageProvider "/home/eric/Bitcasa/data/fractals"
  let consumer = imageWriter
  runEffect $ provider >-> consumer
  --imageClosures <- readImagesFromDirectory "/home/eric/Bitcasa/data/fractals"
  --let firstImage = next $ runEffect imageClosures
  --firstImage : others <- imageClosures
  --Just fI <- firstImage
  --runIL $ writeImage "/home/eric/Downloads/fractal2.png" fI
  putStrLn "Done"