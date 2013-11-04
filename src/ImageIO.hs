module ImageIO where

--import Data.Array.Repa.IO.DevIL
import System.Directory
import System.FilePath.Posix
import Data.Char
import Control.Exception
import System.IO.Error
import Codec.Picture
import Text.Printf
--import Data.Array.Repa
--import Data.Array.Repa.Repr.ForeignPtr
--import Data.Array.Repa.Index
--import Data.Word

-- https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

--imageToRepa :: Image -> Array F DIM3 Word8
--imageToRepa image = case image of
--  RGBA (Array F DIM3 Word8) -> undefined


-- Checks if a filename is an image filename for some standard extensions.
isImageFilename :: FilePath -> Bool
isImageFilename filename = extension `elem` [".bmp", ".jpeg", ".jpg", ".png"]
 where extension = map toLower $ takeExtension filename

imageFilesInDirectory :: FilePath -> IO [FilePath]
imageFilesInDirectory directory = do
  allFilenames <- getDirectoryContents directory
  return $ map ((directory ++ "/") ++) $ filter isImageFilename allFilenames

transcodeToPNG :: DynamicImage -> Maybe DynamicImage
transcodeToPNG = undefined

readImageSafe :: FilePath -> IO (Maybe DynamicImage)
readImageSafe filePath = do
  putStrLn $ "Reading " ++ filePath
  eitherImage <- readImage filePath
  --return eitherImage
  case eitherImage of
  	Right image -> case transcodeToPNG image of
  	  Nothing -> do
  	  	putStrLn $ "Could not transcode image to PNG, skipping: " ++ filePath
  	  	return Nothing
  	  transcoded -> return transcoded
  	Left errorMessage -> do
  	  putStrLn $ printf "Failed to read %s with error: %s.\nSkipping." filePath errorMessage
  	  return Nothing
    


  --case readImage filePath of
  --	Left error -> do
  --	  putStrLn $ "Could not read image, skipping: " ++ filePath
  --	  return Nothing
  --  Right image -> return $ Just image 

  --catchAny (fmap Just (runIL (readImage filename))) $ \_ -> do
  --	putStrLn $ "Could not read image, skipping: " ++ filename
  --	return Nothing  