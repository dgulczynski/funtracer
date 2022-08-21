module Geometry
  ( V3 (V3),
    Colour,
    Position,
    Direction,
    Ray (..),
    Rotation,
    Hit (..),
    Shape (..),
    intersect,
    rotateRoll,
    rotateYaw,
    rotatePitch,
    rotatePitchYawRoll,
    schurProduct,
    fromTo,
    extrude,
  )
where

import Data.List (find)
import Linear
  ( Additive ((^+^)),
    Epsilon (..),
    Metric (dot),
    Quaternion,
    V3 (..),
    axisAngle,
    normalize,
    (*^),
    (^*),
  )

type Vector3 = V3 Float

type Colour = Vector3

type Position = Vector3

type Direction = Vector3

type Rotation = Quaternion Float

data Ray = Ray {origin :: Position, direction :: Direction} deriving (Eq, Show)

data Hit = Hit {hitColour :: Colour, hitTime :: Float, hitPoint :: Position, hitNormal :: Direction} deriving (Eq, Show)

data Shape = Sphere
  { sphereCenter :: Position,
    sphereRadius :: Float,
    sphereColour :: Colour
  }
  deriving (Show)

solveQuadratic :: (Ord a, Epsilon a, Floating a) => a -> a -> a -> [a]
solveQuadratic a b c
  | nearZero d = [-b / (2 * a)]
  | d > 0 = [(-b - d') / (2 * a), (-b + d') / (2 * a)]
  | otherwise = []
  where
    d = b * b - 4 * a * c
    d' = sqrt d

instance Ord Hit where
  (Hit {hitTime = t1}) `compare` (Hit {hitTime = t2}) = t1 `compare` t2

intersect :: Shape -> Ray -> Maybe Hit
intersect sphere ray =
  hit <$> intersectSphere (sphereCenter sphere) (sphereRadius sphere) ray
  where
    hit time =
      Hit
        { hitTime = time,
          hitColour = sphereColour sphere,
          hitPoint = rayHitPoint,
          hitNormal = normalize $ rayHitPoint - sphereCenter sphere
        }
      where
        rayHitPoint = origin ray ^+^ (direction ray ^* time)

intersectSphere :: Position -> Float -> Ray -> Maybe Float
intersectSphere center radius ray = find (> 0) (solveQuadratic a b c)
  where
    oc = origin ray - center
    a = direction ray `dot` direction ray
    b = 2 * oc `dot` direction ray
    c = oc `dot` oc - radius * radius

rotatePitch :: Float -> Rotation
rotatePitch = axisAngle (V3 1 0 0)

rotateYaw :: Float -> Rotation
rotateYaw = axisAngle (V3 0 1 0)

rotateRoll :: Float -> Rotation
rotateRoll = axisAngle (V3 0 0 1)

rotatePitchYawRoll :: Float -> Float -> Float -> Rotation
rotatePitchYawRoll pitch yaw roll = rotatePitch pitch * rotateYaw yaw * rotateRoll roll

schurProduct :: Num a => V3 a -> V3 a -> V3 a
schurProduct (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 * x2) (y1 * y2) (z1 * z2)

fromTo :: Position -> Position -> Ray
fromTo from to = Ray {origin = from, direction = normalize $ to - from}

extrude :: Position -> Direction -> Position
extrude point normal = point ^+^ (0.01 *^ normal)