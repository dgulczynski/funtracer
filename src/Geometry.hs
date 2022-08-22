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
  ( Additive (..),
    Epsilon (..),
    Metric (..),
    Quaternion,
    V3 (..),
    axisAngle,
    cross,
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

data Shape
  = Sphere Colour Position Float
  | Triangle Colour Position Position Position
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

toHit :: Colour -> (Position -> Direction) -> Ray -> Float -> Hit
toHit colour pointToNormal ray time =
  Hit
    { hitTime = time,
      hitColour = colour,
      hitPoint = rayHitPoint,
      hitNormal = pointToNormal rayHitPoint
    }
  where
    rayHitPoint = origin ray ^+^ (direction ray ^* time)

intersect :: Shape -> Ray -> Maybe Hit
intersect shape ray = toHit (colour shape) (pointToNormal shape) ray <$> intersect' shape ray
  where
    intersect' (Sphere _ center radius) = intersectSphere center radius
    intersect' (Triangle _ v0 v1 v2) = intersectTriangle v0 v1 v2
    colour (Sphere c _ _) = c
    colour (Triangle c _ _ _) = c
    pointToNormal (Sphere _ center _) hit = normalize (hit ^-^ center)
    pointToNormal (Triangle _ v0 v1 v2) _ = normalize ((v1 - v0) `cross` (v2 - v0))

intersectSphere :: Position -> Float -> Ray -> Maybe Float
intersectSphere center radius ray = find (> 0) (solveQuadratic a b c)
  where
    oc = origin ray - center
    a = direction ray `dot` direction ray
    b = 2 * oc `dot` direction ray
    c = oc `dot` oc - radius * radius

intersectTriangle :: Position -> Position -> Position -> Ray -> Maybe Float
intersectTriangle v0 v1 v2 ray
  | nearZero det = Nothing
  | u < 0 || u > 1 = Nothing
  | v < 0 || u + v > 1 = Nothing
  | t < 0 = Nothing
  | otherwise = Just t
  where
    v0v1 = v1 - v0
    v0v2 = v2 - v0
    pvec = direction ray `cross` v0v2
    det = v0v1 `dot` pvec
    invDet = 1 / det
    tvec = origin ray - v0
    u = (tvec `dot` pvec) * invDet
    qvec = tvec `cross` v0v1
    v = (direction ray `dot` qvec) * invDet
    t = (v0v2 `dot` qvec) * invDet

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
fromTo from to = Ray {origin = from, direction = normalize $ to ^-^ from}

extrude :: Position -> Direction -> Position
extrude point dir = point ^+^ (0.0001 *^ dir)