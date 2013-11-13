{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck
import Codec.Picture
import Control.Applicative
--import ImageIO
import TangramMisc
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Memo

instance Arbitrary PixelRGBA8 where
  arbitrary = PixelRGBA8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = generateImage <$> pixel <*> dimension <*> dimension
   where 
   	pixel = (\pixel' -> \_ _ -> pixel') <$> arbitrary
   	dimension = choose (500, 1000)

data ImageDimensions = ImageDimensions Int Int deriving Show

instance Arbitrary ImageDimensions where
  arbitrary = ImageDimensions <$> choose (500, 1000) <*> choose (500, 1000)

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