module ImagePool where

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

type ImagePool = Pipe ImageRGBA8 ImageRGBA8 IO ()

