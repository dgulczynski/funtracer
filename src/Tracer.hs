module Tracer (renderScene) where

import Data.Maybe (mapMaybe)
import Geometry
  ( Colour,
    Direction,
    Hit (..),
    Position,
    Ray (..),
    Shape,
    V3 (V3),
    extrude,
    fromTo,
    intersect,
    schurProduct,
  )
import Linear
  ( Additive (zero),
    Metric (distance, dot),
    normalize,
    rotate,
    sumV,
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

intersectScene :: [Shape] -> Ray -> Maybe Hit
intersectScene sceneGeometry ray = maybeMinimum $ mapMaybe (`intersect` ray) sceneGeometry

traceShadowRay :: [Shape] -> Light -> Ray -> Bool
traceShadowRay scene light shadowRay =
  maybe False hitTest (intersectScene scene shadowRay)
  where
    hitTest (Hit {hitTime = t}) = t < lightDistance
    lightDistance = origin shadowRay `distance` lightPosition light

traceRay :: Scene -> Ray -> Colour
traceRay (Scene {geometry = sceneGeometry, lights = sceneLights}) ray =
  maybe zero pixelColor $ intersectScene sceneGeometry ray
  where
    pixelColor (Hit {hitColour = c, hitPoint = p, hitNormal = n}) = c `schurProduct` sumV (map (illuminate p n) sceneLights)
    illuminate point normal light = if traceShadowRay sceneGeometry light shadowRay then zero else illumination
      where
        shadowRay = (point `extrude` normal) `fromTo` lightPos
        illumination = lightAtPoint light point normal
        lightPos = lightPosition light

renderScene :: Scene -> Camera -> (Int, Int) -> [[Colour]]
renderScene scene camera (screenWidth, screenHeight) = traceRay scene <$$> rays
  where
    rays = raysOfScreen camera (screenWidth, screenHeight)
