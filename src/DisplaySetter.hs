module DisplaySetter where

import Pipes
import Pipes.Concurrent
import Codec.Picture
import Control.Concurrent
import Control.Monad

import Tangram
import Tree

type DisplaySetter = Consumer Tangram IO ()

debugDisplaySetter :: FilePath -> DisplaySetter
debugDisplaySetter filePath = forever $ do
  tangram <- await
  case tangram of
    Leaf image -> do
      lift $ putStrLn "Writing"
      lift $ writePng filePath image
      lift $ threadDelay 400000
    _ -> undefined
