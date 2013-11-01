import Data.Array.Repa.IO.DevIL
import System.Directory
import System.FilePath.Posix
import Data.Char
import Control.Exception
import Data.Maybe

-- https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

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

-- Reads all the images in a directory.
readImagesFromDirectory :: FilePath -> IO [Image]
readImagesFromDirectory directory = do
  filenames <- imageFilesInDirectory directory
  unflat <- sequence $ map readImageSafe filenames
  return $ catMaybes unflat

main :: IO ()
main = do
  images <- readImagesFromDirectory "/home/eric/Bitcasa/data/fractals"
  let firstImage : others = images
  runIL $ writeImage "/home/eric/Downloads/fractal2.png" firstImage
  putStrLn "Done"