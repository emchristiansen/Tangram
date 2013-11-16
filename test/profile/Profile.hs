{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck
import Codec.Picture
import Control.Applicative
--import ImageIO
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Memo
import Control.Monad.Random

import System
import BruteTangramMaker
import RolloutTangramMaker

import TestUtil

mtwice :: Monad m => (a -> m a) -> (a -> m a)
mtwice function = \input -> do
  output <- function input
  function output

constraints = Constraints (RectangleSize 1000 500) 300 800 1.02 1

main :: IO ()

main = do
  let pixel = PixelRGBA8 0 0 0 0
  let image = generateImage (\_ _ -> pixel) 300 300
  let images = take 4 $ repeat image
  --images <- replicateM 1 $ liftM head $ sample' arbitrary
  legalTangramMaybe <- evalRandIO $ imagesToTangram constraints images
  putStrLn $ show $ legalTangramMaybe

--main = do
--  images <- replicateM 4 $ liftM head $ sample' arbitrary
--  let tangrams = startEvalMemo $ allTangrams images
--  putStrLn "Start"
--  --putStrLn $ show $ length tangrams
--  mapM_ (putStrLn . show) tangrams
--  putStrLn "Done"

--main = do
--  images <- replicateM 3 $ liftM head $ sample' arbitrary
--  let replicated = mtwice $ mtwice iterateTangrams
--  let tangrams = startEvalMemo $ replicated $ map Leaf images :: [Tangram]
--  --putStrLn $ show $ last tangrams
--  putStrLn $ show $ tangrams !! 18
--  --mapM_ (putStrLn . show) tangrams  