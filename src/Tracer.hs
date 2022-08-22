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

data Light = PointLight {lightColour :: Colour, lightIntensity :: Float, lightPosition :: Position}

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
  colour ^* illumination
  where
    illumination = costheta * intensity / (r * r)
    costheta = max 0 $ normal `dot` pl
    pl = normalize $ position - point
    r = position `distance` point

intersectScene :: [Shape] -> Ray -> Maybe Hit
intersectScene objects ray =
  case mapMaybe (`intersect` ray) objects of
    [] -> Nothing
    hits -> Just $ minimum hits

trace :: [Shape] -> [Light] -> Ray -> Colour
trace scene lights ray = maybe zero pixelColor $ intersectScene scene ray
  where
    pixelColor (Hit {hitColour = c, hitPoint = p, hitNormal = n}) = c `schurProduct` sumV (map (illuminate p n) lights)
    illuminate point normal light =
      case intersectScene scene ((point `extrude` normal) `fromTo` lightPos) of
        Nothing -> illumination
        Just (Hit {hitTime = t}) -> if t < lightDistance then zero else illumination
      where
        illumination = lightAtPoint light point normal
        lightDistance = point `distance` lightPos
        lightPos = lightPosition light

renderScene :: (Int, Int) -> Picture
renderScene (screenWidth, screenHeight) = bitmapOfByteString screenWidth screenHeight (BitmapFormat TopToBottom PxRGBA) bitmapData True
  where
    bitmapData = pack $ concatMap (colourTo24BitRGBA . trace scene lights) $ concat rays
    rays = raysOfScreen (V3 0 0 (-8)) (rotatePitchYawRoll 0 0 0) 1 45 (screenWidth, screenHeight)
    scene =
      [ Sphere white (V3 1 (-1.3) 0.2) 0.7,
        Sphere white (V3 (-0.9) (-1) (-0.4)) 1,
        Triangle white (V3 (-2) (-2) (-2)) (V3 (-2) (-2) 2) (V3 2 (-2) (-2)), -- floor
        Triangle white (V3 2 (-2) (-2)) (V3 (-2) (-2) 2) (V3 2 (-2) 2), --       floor
        Triangle white (V3 (-2) 2 2) (V3 (-2) 2 (-2)) (V3 2 2 (-2)), -- ceiling
        Triangle white (V3 (-2) 2 2) (V3 2 2 (-2)) (V3 2 2 2), --       ceiling
        Triangle green (V3 2 2 (-2)) (V3 2 (-2) (-2)) (V3 2 2 2), -- left wall
        Triangle green (V3 2 (-2) (-2)) (V3 2 (-2) 2) (V3 2 2 2), -- left wall
        Triangle red (V3 (-2) (-2) 2) (V3 (-2) (-2) (-2)) (V3 (-2) 2 2), -- right wall
        Triangle red (V3 (-2) (-2) (-2)) (V3 (-2) 2 (-2)) (V3 (-2) 2 2), -- right wall
        Triangle blue (V3 2 (-2) 2) (V3 (-2) (-2) 2) (V3 2 2 2), --  back wall
        Triangle blue (V3 2 2 2) (V3 (-2) (-2) 2) (V3 (-2) 2 2.1) -- back wall
      ]
    lights =
      [ PointLight white 5 (V3 0 1.5 0),
        PointLight white 25 (V3 0 1.9 (-10))
      ]
    white = V3 1 1 1
    red = V3 0.9 0.2 0.2
    green = V3 0.2 0.9 0.2
    blue = V3 0.2 0.2 0.9
