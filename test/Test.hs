{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Framework
--import Test.QuickCheck
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

--data ImageDimension = ImageDimension Int deriving Show

--instance Arbitrary ImageDimension where
--  arbitrary = ImageDimension <$> choose (500, 1000)

prop_legalCrops :: ImageDimensions -> Bool
prop_legalCrops (ImageDimensions width height) = 
  length (legalCrops width height) > 0

test_legalCrops :: IO ()
test_legalCrops = do
  let crops = legalCrops 500 600
  --mapM_ (putStrLn . show) crops
  putStrLn $ show $ length crops

prop_legalRescalings :: ImageDimensions -> Bool
prop_legalRescalings (ImageDimensions width height) = 
  length (legalRescalings width height) > 0

--test_legalTangramSizes_memo :: IO ()
--test_legalTangramSizes_memo = do
--  image <- liftM head $ sample' arbitrary
--  putStrLn $ show $ map numSizes $ startEvalMemo $ replicateM 20 $ legalTangramSizes $ Leaf image

--test_legalTangramSizes :: IO ()
--test_legalTangramSizes = do
--  imageLeft <- (liftM head) $ sample' arbitrary
--  imageRight <- (liftM head) $ sample' arbitrary
--  let leftSizes = startEvalMemo $ legalTangramSizes (Leaf imageLeft)
--  let rightSizes = startEvalMemo $ legalTangramSizes (Leaf imageRight)
--  putStrLn $ show $ numSizes leftSizes
--  putStrLn $ show $ numSizes rightSizes
--  let sizes = startEvalMemo $ legalTangramSizes (Horizontal (Leaf imageLeft) (Leaf imageRight))
--  putStrLn $ show $ numSizes sizes

--test_iterateTangrams :: IO ()
--test_iterateTangrams = do
--  images <- replicateM 2 $ liftM head $ sample' arbitrary
--  let tangrams = startEvalMemo $ iterateTangrams $ map Leaf images
--  putStrLn $ show $ length tangrams
--  mapM_ (putStrLn . show) tangrams
--  let tangrams' = startEvalMemo $ iterateTangrams tangrams
--  putStrLn $ show $ length tangrams'
--  mapM_ (putStrLn . show) tangrams'
--  let tangrams'' = startEvalMemo $ iterateTangrams tangrams'
--  putStrLn $ show $ length tangrams''
--  mapM_ (putStrLn . show) tangrams''  

--test_allTangrams :: IO ()
--test_allTangrams = do
--  images <- replicateM 5 $ liftM head $ sample' arbitrary
--  let tangrams = startEvalMemo $ allTangrams images
--  putStrLn "Start"
--  --putStrLn $ show $ length tangrams
--  mapM_ (putStrLn . show) tangrams
--  putStrLn "Done"

--prop_legalImageSizes :: ImageDimension -> ImageDimension -> Bool
--prop_legalImageSizes (ImageDimension width) (ImageDimension height) =
--  length (legalImageSizes width height) > 0

--prop_legalTangramSizes :: (Image PixelRGBA8) -> Bool
--prop_legalTangramSizes image = numSizes > 0
-- where 
--  sizes = legalTangramSizes (Leaf image)
--  numSizes = length $ MM.keys $ fst sizes

--myReverse :: [a] -> [a]
--myReverse []     = []
--myReverse [x]    = [x]
--myReverse (x:xs) = (myReverse xs) ++ [x]

--test_nonEmpty = do assertEqual [1] (myReverse [1])
--                   putStrLn "here"
--                   assertEqual [3,2,1] (myReverse [1,2,3])

--test_empty = assertEqual ([] :: [Int]) (myReverse [])

--prop_reverse :: [Int] -> Bool
--prop_reverse xs = xs == (myReverse (myReverse xs))

main :: IO ()
main = htfMain htf_thisModulesTests