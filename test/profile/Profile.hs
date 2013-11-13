{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck
import Codec.Picture
import Control.Applicative
--import ImageIO
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Memo

import System
import BruteTangramMaker

import TestUtil

mtwice :: Monad m => (a -> m a) -> (a -> m a)
mtwice function = \input -> do
  output <- function input
  function output

main :: IO ()

--main = do
--  images <- replicateM 4 $ liftM head $ sample' arbitrary
--  let tangrams = startEvalMemo $ allTangrams images
--  putStrLn "Start"
--  --putStrLn $ show $ length tangrams
--  mapM_ (putStrLn . show) tangrams
--  putStrLn "Done"

main = do
  images <- replicateM 4 $ liftM head $ sample' arbitrary
  let replicated = mtwice $ mtwice iterateTangrams
  let tangrams = startEvalMemo $ replicated $ map Leaf images
  mapM_ (putStrLn . show) tangrams  