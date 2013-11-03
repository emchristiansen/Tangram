module ImageIO where

import Data.Array.Repa.IO.DevIL
import System.Directory
import System.FilePath.Posix
import Data.Char
import Control.Exception
import System.IO.Error

-- https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

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
  catchAny (fmap Just (runIL (readImage filename))) $ \_ -> do
  	putStrLn $ "Could not read image, skipping: " ++ filename
  	return Nothing  