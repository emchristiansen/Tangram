{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.QuickCheck
import Codec.Picture
import Control.Applicative
import ImageIO
import TangramMisc
import qualified Data.MultiMap as MM

instance Arbitrary PixelRGBA8 where
  arbitrary = PixelRGBA8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = generateImage <$> pixel <*> dimension <*> dimension
   where 
   	pixel = (\pixel -> \_ _ -> pixel) <$> arbitrary
   	dimension = choose (500, 1000)

instance Show (Image PixelRGBA8) where
  show image = "Show image"

data ImageDimensions = ImageDimensions Int Int deriving Show

instance Arbitrary ImageDimensions where
  arbitrary = ImageDimensions <$> choose (500, 1000) <*> choose (500, 1000)

--data ImageDimension = ImageDimension Int deriving Show

--instance Arbitrary ImageDimension where
--  arbitrary = ImageDimension <$> choose (500, 1000)

prop_legalCrops :: ImageDimensions -> Bool
prop_legalCrops (ImageDimensions width height) = 
  length (legalCrops width height) > 0

prop_legalRescalings :: ImageDimensions -> Bool
prop_legalRescalings (ImageDimensions width height) = 
  length (legalRescalings width height) > 0

--prop_legalImageSizes :: ImageDimension -> ImageDimension -> Bool
--prop_legalImageSizes (ImageDimension width) (ImageDimension height) =
--  length (legalImageSizes width height) > 0

--prop_legalTangramSizes :: (Image PixelRGBA8) -> Bool
--prop_legalTangramSizes image = numSizes > 0
-- where 
--  sizes = legalTangramSizes (Leaf image)
--  numSizes = length $ MM.keys $ fst sizes

myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

test_nonEmpty = do assertEqual [1] (myReverse [1])
                   assertEqual [3,2,1] (myReverse [1,2,3])

test_empty = assertEqual ([] :: [Int]) (myReverse [])

prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == (myReverse (myReverse xs))

main = htfMain htf_thisModulesTests