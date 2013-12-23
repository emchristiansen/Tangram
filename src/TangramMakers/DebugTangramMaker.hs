module DebugTangramMaker where

import Pipes
import Pipes.Concurrent
import Codec.Picture
import Control.Concurrent (threadDelay)
import Control.Monad

import Tangram
import Tree
import ImageRGBA8
import Util
import TangramMaker

debugTangramMaker :: TangramMaker
debugTangramMaker = forever $ do
  image <- await
  switch <- lift $ randomBool
  case switch of
    False -> do
      lift $ putStrLn "Simulating failure"
      yield $ Left image
    True -> do
      lift $ putStrLn "Simulating success"
      yield $ Right $ Leaf image
