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
  )
where

import Data.List (find)
import Linear
  ( Epsilon (..),
    Metric (dot),
    Quaternion,
    V3 (..),
    axisAngle,
  )

type Vector3 = V3 Float

type Colour = Vector3

type Position = Vector3

type Direction = Vector3

type Rotation = Quaternion Float

data Ray = Ray {origin :: Position, direction :: Direction} deriving (Eq, Show)

data Hit = Hit {hitColour :: Colour, hitTime :: Float} deriving (Eq, Show)

data Shape = Sphere
  { sphereCenter :: Position,
    sphereRadius :: Float,
    sphereColour :: Colour
  }
  deriving (Show)

solveQuadratic :: (Ord a, Epsilon a, Floating a) => a -> a -> a -> [a]
solveQuadratic a b c
  | nearZero d = [- b / (2 * a)]
  | d > 0 = [(- b - d') / (2 * a), (- b + d') / (2 * a)]
  | otherwise = []
  where
    d = b * b - 4 * a * c
    d' = sqrt d

instance Ord Hit where
  (Hit {hitTime = t1}) `compare` (Hit {hitTime = t2}) = t1 `compare` t2

intersect :: Shape -> Ray -> Maybe Hit
intersect sphere ray =
  hit <$> t
  where
    hit time = Hit {hitTime = time, hitColour = sphereColour sphere}
    t = find (> 0) (solveQuadratic a b c)
    oc = origin ray - sphereCenter sphere
    a = direction ray `dot` direction ray
    b = 2 * oc `dot` direction ray
    c = oc `dot` oc - sphereRadius sphere * sphereRadius sphere

rotatePitch :: Float -> Rotation
rotatePitch = axisAngle (V3 1 0 0)

rotateYaw :: Float -> Rotation
rotateYaw = axisAngle (V3 0 1 0)

rotateRoll :: Float -> Rotation
rotateRoll = axisAngle (V3 0 0 1)

rotatePitchYawRoll :: Float -> Float -> Float -> Rotation
rotatePitchYawRoll pitch yaw roll = rotatePitch pitch * rotateYaw yaw * rotateRoll roll
