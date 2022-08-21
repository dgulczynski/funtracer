module Tracer (renderScene) where

import Data.ByteString (pack)
import Data.List ()
import Data.Maybe (mapMaybe)
import Geometry
  ( Colour,
    Hit (Hit, hitColour, hitTime),
    Position,
    Ray (..),
    Rotation,
    Shape (..),
    V3 (V3),
    intersect,
    rotatePitchYawRoll,
  )
import Graphics.Gloss
  ( BitmapFormat (BitmapFormat),
    Picture,
    PixelFormat (PxRGBA),
    RowOrder (TopToBottom),
    bitmapOfByteString,
  )
import Linear (normalize, rotate, (^*))
import Utils (colourTo24BitRGBA)

raysOfScreen :: Position -> Rotation -> Float -> Float -> (Int, Int) -> [[Ray]]
raysOfScreen cameraPosition rotation screenDistance vfov (width, height) =
  [[rayAtPixel x y | x <- [1 .. width]] | y <- [1 .. height]]
  where
    transform = rotate rotation
    ratio = fromIntegral width / fromIntegral height
    screenHeight = screenDistance * 2 * tan (vfov * (pi / 360))
    screenWidth = ratio * screenHeight
    rayAtPixel x y = Ray {origin = cameraPosition, direction = normalize delta}
      where
        pixelIndexToOffset i range = fromIntegral i / fromIntegral range
        dx = (pixelIndexToOffset x width - 0.5) * screenWidth
        dy = (pixelIndexToOffset y height - 0.5) * screenHeight
        delta = transform $ V3 dx dy screenDistance

intersectScene :: [Shape] -> Ray -> Colour
intersectScene objects ray =
  case mapMaybe (`intersect` ray) objects of
    [] -> V3 0 0 0
    hits -> pixelColor $ minimum hits
  where
    pixelColor (Hit {hitColour = c, hitTime = t}) = c ^* sqrt (1 / t)

renderScene :: (Int, Int) -> Picture
renderScene (screenWidth, screenHeight) = bitmapOfByteString screenWidth screenHeight (BitmapFormat TopToBottom PxRGBA) bitmapData True
  where
    trace = intersectScene scene
    bitmapData = pack $ concatMap (colourTo24BitRGBA . trace) $ concat rays
    rays = raysOfScreen (V3 0 0 (-3)) (rotatePitchYawRoll 0.1 0.1 0) 1 60 (screenWidth, screenHeight)
    scene =
      [ Sphere {sphereCenter = V3 0 2 3, sphereRadius = 1, sphereColour = V3 0.0 1.0 1.0},
        Sphere {sphereCenter = V3 (-1) 1 7, sphereRadius = 3, sphereColour = V3 1 0.1 0.7},
        Sphere {sphereCenter = V3 2 1 10, sphereRadius = 2, sphereColour = V3 0.1 0.7 1},
        Sphere {sphereCenter = V3 0 (-3) 5, sphereRadius = 2, sphereColour = V3 0 0.3 1}
      ]