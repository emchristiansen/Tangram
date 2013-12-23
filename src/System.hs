{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}


module System where

import Control.Monad
import Data.Maybe
import Pipes
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Data.Monoid
import Control.Concurrent.Async
--import Data.List.Utils
{-import Data.List-}
import Codec.Picture
{-import qualified System.Random as R-}
--import qualified Data.Set as Set
import Text.Printf
{-import Data.Tuple-}
{-import qualified Data.Map as M-}
{-import qualified Data.MultiMap as MM-}
{-import Control.Applicative-}
{-import Control.Monad.Memo-}
{-import Data.Function  -}
{-import Control.Arrow ((&&&))-}
import Control.Lens

import ImageUtil
import PipeUtil
import Util


import ImageProducer
import ImagePool
import TangramMaker
import DisplaySetter

takeLeftPipe :: Pipe (Either a b) a IO ()
takeLeftPipe = P.map (\x -> lefts [x]) >-> P.concat

takeRightPipe :: Pipe (Either a b) b IO ()
takeRightPipe = P.map (\x -> rights [x]) >-> P.concat

data System = System {
  _systemImageProducerL :: ImageProducer,
  _systemImagePoolL :: ImagePool,
  _systemTangramMakerL :: TangramMaker,
  _systemDisplaySetterL :: DisplaySetter
}
makeFields ''System

runSystem :: System -> IO ()
runSystem (System imageProducer imagePool tangramMaker displaySetter) = do
  (imagePoolOutput, imagePoolInput) <- spawn Single
  (tangramFailureOutput, tangramFailureInput) <- spawn Unbounded
  (tangramSuccessOutput, tangramSuccessInput) <- spawn Single

  let fetchImages = imageProducer >-> toOutput imagePoolOutput

  let input = fromInput imagePoolInput
  let output = toOutput $ tangramFailureOutput <> tangramSuccessOutput
  let makeTangrams = input >-> imagePool >-> tangramMaker >-> output

  let recycleImages = fromInput tangramFailureInput >-> takeLeftPipe >-> toOutput imagePoolOutput

  let displayTangrams = fromInput tangramSuccessInput >-> takeRightPipe >-> displaySetter

  --a <- async $ runEffect fetchImages
  --b <- async $ runEffect makeTangrams
  --c <- async $ runEffect recycleImages
  --d <- async $ runEffect displayTangrams

  --mapM_ wait [a, b, c, d]

  jobsFinished <- mapM (async . runEffect) [
    fetchImages, 
    makeTangrams, 
    recycleImages, 
    displayTangrams]

  mapM_ wait jobsFinished

  return ()
