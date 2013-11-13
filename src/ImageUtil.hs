{-# OPTIONS_GHC -fno-warn-orphans #-}

module ImageUtil where

import System.Directory
import System.FilePath.Posix
import Data.Char
--import Control.Exception
--import System.IO.Error
import Codec.Picture
import Codec.Picture.Types
import Text.Printf
import Control.Arrow ((&&&))
--import Data.ByteString.Lazy (toStrict)
--import System.IO
--import Control.Concurrent (threadDelay)
--import System.Random

type ImageRGBA8 = Image PixelRGBA8

instance Show (Image PixelRGBA8) where
  show image = show $ (imageWidth image, imageHeight image)

_properties = imageWidth &&& imageHeight &&& imageData

-- TODO: Surely I don't need this given I have defined Ord below.
instance Eq (Image PixelRGBA8) where
  left == right = _properties left == _properties right

instance Ord (Image PixelRGBA8) where
  left `compare` right = _properties left `compare` _properties right

-- Checks if a filename is an image filename for some standard extensions.
isImageFilename :: FilePath -> Bool
isImageFilename filename = extension `elem` [".bmp", ".jpeg", ".jpg", ".png"]
 where extension = map toLower $ takeExtension filename

imageFilesInDirectory :: FilePath -> IO [FilePath]
imageFilesInDirectory directory = do
  allFilenames <- getDirectoryContents directory
  return $ map ((directory ++ "/") ++) $ filter isImageFilename allFilenames

transcodeToImageRGBA8 :: DynamicImage -> Maybe (ImageRGBA8)
transcodeToImageRGBA8 dynamicImage = case dynamicImage of
  ImageY8 image -> Just $ promoteImage image
  ImageYA8 image -> Just $ promoteImage image
  ImageRGB8 image -> Just $ promoteImage image
  ImageRGBA8 image -> Just $ promoteImage image
  ImageYCbCr8 image -> Just $ 
    promoteImage (convertImage image :: Image PixelRGB8)
  ImageCMYK8 image -> Just $ 
    promoteImage (convertImage image :: Image PixelRGB8)
  _ -> Nothing

-- Attempts to read an `ImageRGBA8` from the given `FilePath`.
readImageSafe :: FilePath -> IO (Maybe ImageRGBA8)
readImageSafe filePath = do
  putStrLn $ "Reading " ++ filePath
  eitherImage <- readImage filePath
  case eitherImage of
  	Right image -> case transcodeToImageRGBA8 image of
  	  Nothing -> do
  	    putStrLn $ "Could not transcode image to PNG, skipping: " ++ filePath
  	    return Nothing
  	  transcoded -> return transcoded
  	Left errorMessage -> do
  	  let formatString = "Failed to read %s with error: %s.\nSkipping."
  	  putStrLn $ printf formatString filePath errorMessage
  	  return Nothing