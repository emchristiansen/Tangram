{-# OPTIONS_GHC -fno-warn-orphans #-}

module ImageIO where

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

-- TODO: Surely I don't need this given I have defined Ord below.
instance Eq (Image PixelRGBA8) where
  left == right = properties left == properties right
   where properties = imageWidth &&& imageHeight &&& imageData

instance Ord (Image PixelRGBA8) where
  left `compare` right = properties left `compare` properties right
   where properties = imageWidth &&& imageHeight &&& imageData

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

---- Transcode an image to the PNG format.
---- Unfortunately, this method doesn't seem to work as frequently as the
---- IO-based method below.
--transcodeToPNG :: DynamicImage -> Maybe DynamicImage
--transcodeToPNG image = case encodeDynamicPng image of
--  Left _ -> Nothing
--  Right byteString -> case decodePng (toStrict byteString) of
--  	Left _ -> Nothing
--  	Right transcoded -> Just transcoded

---- It's a bit lame, but it seems writing the image to disk and reading back
---- is the most effective way to transcode the image.
--ioTranscodeToPNG :: DynamicImage -> IO (Maybe DynamicImage)
--ioTranscodeToPNG image = do
--  random <- getStdGen
--  let nextInt = abs $ fst $ next random
--  let filePath = printf "/tmp/tangramTranscoded%d.png" nextInt
--  savePngImage filePath image
--  eitherTranscoded <- readImage filePath
--  removeFile filePath
--  case eitherTranscoded of
--  	Left _ -> return Nothing
--  	-- Now let's make sure we can "transcode" without using IO.
--  	Right transcoded -> return $ transcodeToPNG transcoded

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

readImageSafe :: FilePath -> IO (Maybe ImageRGBA8)
readImageSafe filePath = do
  putStrLn $ "Reading " ++ filePath
  eitherImage <- readImage filePath
  case eitherImage of
  	--Right image -> return $ Just image
  	Right image -> case transcodeToImageRGBA8 image of
  	  Nothing -> do
  	    putStrLn $ "Could not transcode image to PNG, skipping: " ++ filePath
  	    return Nothing
  	  transcoded -> return transcoded
  	Left errorMessage -> do
  	  let formatString = "Failed to read %s with error: %s.\nSkipping."
  	  putStrLn $ printf formatString filePath errorMessage
  	  return Nothing