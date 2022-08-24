module Tracer (renderScene) where

import Data.Maybe (mapMaybe)
import Geometry
  ( Colour,
    Direction,
    Hit (..),
    Position,
    Ray (..),
    V3 (V3),
    extrude,
    fromTo,
    intersect,
    reflect,
    schurProduct,
  )
import Linear
  ( Additive (zero),
    Metric (distance, dot),
    normalize,
    rotate,
    sumV,
    (*^),
    (^*),
  )
import Scene
  ( Camera (..),
    Light (..),
    Scene (..),
  )
import Utils (maybeMinimum, (<$$>))

raysOfScreen :: Camera -> (Int, Int) -> [[Ray]]
raysOfScreen camera (width, height) =
  [[rayAtPixel x y | x <- [1 .. width]] | y <- [1 .. height]]
  where
    transform = rotate (cameraRotation camera)
    ratio = fromIntegral width / fromIntegral height
    screenHeight = screenDistance * 2 * tan (cameraVFov camera * (pi / 360))
    screenWidth = ratio * screenHeight
    screenDistance = cameraDistance camera
    rayAtPixel x y = Ray {origin = cameraPosition camera, direction = normalize delta}
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

intersectScene :: Scene -> Ray -> Maybe Hit
intersectScene scene ray = maybeMinimum $ mapMaybe (`intersect` ray) (geometry scene)

traceShadowRay :: Scene -> Light -> Ray -> Bool
traceShadowRay scene light shadowRay =
  maybe False hitTest (intersectScene scene shadowRay)
  where
    hitTest (Hit {hitTime = t}) = t < lightDistance
    lightDistance = origin shadowRay `distance` lightPosition light

traceRay :: Int -> Scene -> Ray -> Colour
traceRay bounces scene ray
  | bounces < 0 = zero
  | otherwise = maybe zero pixelColor $ intersectScene scene ray
  where
    pixelColor hit = hitColor hit + traceSecondaryRay 0.3 hit
    hitColor (Hit {hitColour = c, hitPoint = p, hitNormal = n}) = c `schurProduct` sumV (map (illuminate p n) (lights scene))
    traceSecondaryRay multiplier (Hit {hitPoint = p, hitNormal = n}) = multiplier *^ traceRay (bounces - 1) scene secondaryRay
      where
        secondaryRay = Ray {origin = extrude p n, direction = reflect n (direction ray)}
    illuminate point normal light = if traceShadowRay scene light shadowRay then zero else illumination
      where
        shadowRay = (point `extrude` normal) `fromTo` lightPos
        illumination = lightAtPoint light point normal
        lightPos = lightPosition light

renderScene :: Scene -> Camera -> (Int, Int) -> [[Colour]]
renderScene scene camera (screenWidth, screenHeight) = traceRay 3 scene <$$> rays
  where
    rays = raysOfScreen camera (screenWidth, screenHeight)
