module TangramMaker where

import Pipes
import Pipes.Concurrent
import Codec.Picture
import Control.Concurrent (threadDelay)
import Control.Monad

import SizedTangram
import Tree
import ImageRGBA8
import Util

type TangramMaker = Pipe ImageRGBA8 (Either ImageRGBA8 Tangram) IO ()

