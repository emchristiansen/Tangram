module TestUtil where

import Codec.Picture
import Control.Applicative
import Test.QuickCheck

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