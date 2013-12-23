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

-- Consumes images and produces tangrams.
-- When everything goes well, the output is a `Right Tangram`.
-- Some images are rejects, which get returned as failures (`Left ImageRGBA8`).
type TangramMaker = Pipe ImageRGBA8 (Either ImageRGBA8 Tangram) IO ()

