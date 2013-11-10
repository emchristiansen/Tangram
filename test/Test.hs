{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.QuickCheck
import Codec.Picture
import Control.Applicative
import ImageIO

data MyInt = MyInt Int

instance Arbitrary MyInt where
  arbitrary = do
  	int <- choose (10, 20)
  	return $ MyInt int

instance Arbitrary PixelRGBA8 where
  arbitrary = PixelRGBA8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = generateImage <$> pixel <*> dimension <*> dimension
   where 
   	pixel = (\pixel -> \_ _ -> pixel) <$> arbitrary
   	dimension = choose (500, 1000)


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