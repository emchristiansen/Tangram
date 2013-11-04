module ImageIO where

import System.Directory
import System.FilePath.Posix
import Data.Char
--import Control.Exception
--import System.IO.Error
import Codec.Picture
import Text.Printf
import Data.ByteString.Lazy (toStrict)

-- https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
--catchAny :: IO a -> (SomeException -> IO a) -> IO a
--catchAny = Control.Exception.catch

--removeIfExists :: FilePath -> IO ()
--removeIfExists fileName = removeFile fileName `catch` handleExists
--  where handleExists e
--          | isDoesNotExistError e = return ()
--          | otherwise = throwIO e

-- Checks if a filename is an image filename for some standard extensions.
isImageFilename :: FilePath -> Bool
isImageFilename filename = extension `elem` [".bmp", ".jpeg", ".jpg", ".png"]
 where extension = map toLower $ takeExtension filename

imageFilesInDirectory :: FilePath -> IO [FilePath]
imageFilesInDirectory directory = do
  allFilenames <- getDirectoryContents directory
  return $ map ((directory ++ "/") ++) $ filter isImageFilename allFilenames

transcodeToPNG :: DynamicImage -> Maybe DynamicImage
transcodeToPNG image = case encodeDynamicPng image of
  Left _ -> Nothing
  Right byteString -> case decodePng (toStrict byteString) of
  	Left _ -> Nothing
  	Right transcoded -> Just transcoded

readImageSafe :: FilePath -> IO (Maybe DynamicImage)
readImageSafe filePath = do
  putStrLn $ "Reading " ++ filePath
  eitherImage <- readImage filePath
  case eitherImage of
  	Right image -> case transcodeToPNG image of
  	  Nothing -> do
  	  	putStrLn $ "Could not transcode image to PNG, skipping: " ++ filePath
  	  	return Nothing
  	  transcoded -> return transcoded
  	Left errorMessage -> do
  	  let formatString = "Failed to read %s with error: %s.\nSkipping."
  	  putStrLn $ printf formatString filePath errorMessage
  	  return Nothing