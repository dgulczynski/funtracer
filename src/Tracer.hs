module Tracer (renderScene) where

import Data.ByteString (pack)
import Data.Maybe (mapMaybe)
import Geometry
  ( Colour,
    Direction,
    Hit (..),
    Position,
    Ray (..),
    Rotation,
    Shape (..),
    V3 (V3),
    extrude,
    fromTo,
    intersect,
    rotatePitchYawRoll,
    schurProduct,
  )
import Graphics.Gloss
  ( BitmapFormat (BitmapFormat),
    Picture,
    PixelFormat (PxRGBA),
    RowOrder (TopToBottom),
    bitmapOfByteString,
  )
import Linear
  ( Additive (zero),
    Metric (distance, dot),
    normalize,
    rotate,
    sumV,
    (^*),
  )
import Utils (colourTo24BitRGBA)

data Light = PointLight {lightPosition :: Position, lightIntensity :: Float, lightColour :: Colour}

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
        dy = (0.5 - pixelIndexToOffset y height) * screenHeight
        delta = transform $ V3 dx dy screenDistance

lightAtPoint :: Light -> Position -> Direction -> Colour
lightAtPoint (PointLight {lightIntensity = intensity, lightColour = colour, lightPosition = position}) point normal =
  colour ^* (cos theta * intensity / (r * r))
  where
    r = distance position point
    pl = normalize $ position - point
    theta = max 0 $ normal `dot` pl

intersectScene :: [Shape] -> Ray -> Maybe Hit
intersectScene objects ray =
  case mapMaybe (`intersect` ray) objects of
    [] -> Nothing
    hits -> Just $ minimum hits

trace :: [Shape] -> [Light] -> Ray -> Colour
trace objects lights ray = maybe zero pixelColor $ intersectScene objects ray
  where
    pixelColor (Hit {hitColour = c, hitPoint = p, hitNormal = n}) = c `schurProduct` sumV (map (illuminate p n) lights)
    illuminate point normal light@(PointLight lightPosition _ _) =
      case intersectScene objects (extrude point normal `fromTo` lightPosition) of
        Nothing -> lightAtPoint light point normal
        Just _ -> zero

renderScene :: (Int, Int) -> Picture
renderScene (screenWidth, screenHeight) = bitmapOfByteString screenWidth screenHeight (BitmapFormat TopToBottom PxRGBA) bitmapData True
  where
    bitmapData = pack $ concatMap (colourTo24BitRGBA . trace scene lights) $ concat rays
    rays = raysOfScreen (V3 0 1.75 0) (rotatePitchYawRoll 0 0 0) 1 60 (screenWidth, screenHeight)
    scene =
      [ Sphere {sphereCenter = V3 0 2 3, sphereRadius = 1, sphereColour = V3 0.0 1.0 1.0},
        Sphere {sphereCenter = V3 (-1) 1 7, sphereRadius = 3, sphereColour = V3 1 0.1 0.7},
        Sphere {sphereCenter = V3 2 1 10, sphereRadius = 2, sphereColour = V3 0.1 0.7 1},
        Sphere {sphereCenter = V3 0 (-3) 5, sphereRadius = 2, sphereColour = V3 0 0.3 1},
        Sphere {sphereCenter = V3 0 (-1000) 0, sphereRadius = 1000, sphereColour = V3 0.5 0.5 0.5}
      ]
    lights =
      [ PointLight (V3 5 5 3) 10 (V3 1 0.1 0.5),
        PointLight (V3 (-3) 4 2) 15 (V3 0.3 0.3 1),
        PointLight (V3 0 3 (-10)) 25 (V3 1 0.5 1)
      ]