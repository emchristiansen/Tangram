module RolloutTangramMaker where

import System.Random (StdGen)
import Control.Monad.Random
import Control.Monad

die :: (RandomGen g) => Rand g Int
die = getRandomR (1,6)

-- A monadic version of scanl.
-- Probably not the most efficient implementation.
scanlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
scanl function start values = foldM function' [start] values
 where 
  -- A modified version of `function` which passes along the yielded values.
  function' as nextB = do
  	nextA <- function (last as) nextB
  	return $ as ++ [nextA]

addToTangram :: (RandomGen g) => Tangram -> Image -> Rand g Tangram
addToTangram tangram image = do
  switch <- getRandom
  return $ if switch then
  	Vertical tangram (Leaf image)
  	Horizontal tangram (Leaf image)

foo :: (RandomGen g) => [Image] -> Rand g [Tangram]
foo images = scanlM addToTangram (Leaf $ head images) $ tail images

--scanlTangrams :: [ImageRGBA8] -> StdGen -> [Tangram]
--scanlTangrams images stdGen = undefined
-- where
--  image : otherImages = images
--  foo (tangram, stdGen') newImage = do
--  	let (nextInt, stdGen'') = next stdGen'
--  	let nextBool = even nextInt
--  	let tangram' = if nextBool then
--      Vertical tangram (Leaf newImage)
--      Horizontal tangram (Leaf newImage)
--    (tangram', stdGen'')
  	
--  scanl () (Leaf image, stdGen) otherImages

--scanlTangrams :: MonadMemo Tangram TangramSizes m => [ImageRGBA8] -> StdGen -> m [Tangram]
--scanlTangrams images stdGen = undefined
-- where
--  image : otherImages = images
--  foo (tangram, stdGen') newImage = do
--  	let (nextInt, stdGen'') = next stdGen'
--  	let nextBool = even nextInt

--  scanl () (Leaf image, stdGen) otherImages