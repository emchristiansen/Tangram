{-# OPTIONS_GHC -fno-warn-orphans #-}

module ImageRGBA8 where

import Codec.Picture
import Control.Arrow
import Data.Word
import Data.Vector.Storable

type ImageRGBA8 = Image PixelRGBA8

instance Show ImageRGBA8 where
  show image = show $ (imageWidth image, imageHeight image)

_properties :: ImageRGBA8 -> (Int, (Int, Vector Word8))
_properties = imageWidth &&& imageHeight &&& imageData

-- TODO: Surely I don't need this given I have defined Ord below.
instance Eq ImageRGBA8 where
  image0 == image1 = _properties image0 == _properties image1 

instance Ord ImageRGBA8 where
  image0 `compare` image1 = _properties image0 `compare` _properties image1
