module Utils (clamp, floatToByte, colourTo24BitRGBA) where

import Data.Word (Word8)
import Geometry (Colour, V3 (V3))

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) = max low . min high

floatToByte :: Float -> Word8
floatToByte x = floor $ clamp (0, 255) (x * 255)

colourTo24BitRGBA :: Colour -> [Word8]
colourTo24BitRGBA (V3 r g b) = [floatToByte r, floatToByte g, floatToByte b, 255]