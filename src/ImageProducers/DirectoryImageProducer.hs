module DirectoryImageProducer where

import Pipes
import Pipes.Concurrent
import Codec.Picture
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Maybe

import Tangram
import Tree
import ImageRGBA8
import ImageUtil
import Util

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
