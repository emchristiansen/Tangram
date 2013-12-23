module MixingImagePool where

import Pipes
import Pipes.Concurrent
import Codec.Picture
import Control.Concurrent (threadDelay)
import Control.Monad
import Text.Printf

import Tangram
import Tree
import ImageRGBA8
import Util

-- If there are any rejected images, yield a rejected image with 50%
-- probability and an unknown image with 50% probability.
-- Otherwise yield an unknown image.
mixingImagePool :: [ImageRGBA8] -> [ImageRGBA8] -> [ImageRGBA8] -> ImagePool
mixingImagePool freshImages rejectedImages knownImages = do
  _ <- lift $ printf "length freshImages: %d\n" $ length freshImages
  _ <- lift $ printf "length rejectedImages: %d\n" $ length rejectedImages
  _ <- lift $ printf "length knownImages: %d\n" $ length knownImages
  case null freshImages of
    -- Make sure we have at least one fresh image.
    True -> do
      image <- await
      case image `elem` knownImages of
        True -> mixingImagePool freshImages (image : rejectedImages) knownImages
        False -> mixingImagePool (image : freshImages) rejectedImages (image :knownImages)
    False -> case rejectedImages of
      -- If we haven't accumulated any rejects, we yield a fresh image.
      [] -> do
        let freshHead : freshTail = freshImages
        yield freshHead
        mixingImagePool freshTail rejectedImages knownImages
      -- If we have rejects, we flip a coin to determine whether to yield
      -- a reject or a new image.
      rejectedHead : rejectedTail -> do
        yieldRejected <- lift $ randomBool
        case yieldRejected of
          True -> do
            yield rejectedHead
            mixingImagePool freshImages rejectedTail knownImages
          False -> do
            let freshHead : freshTail = freshImages
            yield freshHead
            mixingImagePool freshTail rejectedImages knownImages
