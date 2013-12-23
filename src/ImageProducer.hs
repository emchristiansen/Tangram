module ImageProducer where

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

type ImageProducer = Producer ImageRGBA8 IO ()

