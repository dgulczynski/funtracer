module Scene (Scene (..), Light (..), Camera (..), defaultScene, defaultCamera) where

import Geometry
  ( Colour,
    Position,
    Rotation,
    Shape (..),
    V3 (V3),
    rotatePitchYawRoll,
  )

data Light = PointLight {lightColour :: Colour, lightIntensity :: Float, lightPosition :: Position}

data Scene = Scene {geometry :: [Shape], lights :: [Light]}

data Camera = Camera {cameraPosition :: Position, cameraRotation :: Rotation, cameraDistance :: Float, cameraVFov :: Float}

defaultScene :: Scene
defaultScene =
  Scene
    { geometry =
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
        ],
      lights =
        [ PointLight white 5 (V3 0 1.7 (-0.5)),
          PointLight white 25 (V3 0 1.9 (-10))
        ]
    }
  where
    white = V3 1 1 1
    red = V3 0.9 0.2 0.2
    green = V3 0.2 0.9 0.2
    blue = V3 0.2 0.2 0.9

defaultCamera :: Camera
defaultCamera =
  Camera
    { cameraPosition = V3 0 0 (-8),
      cameraRotation = rotatePitchYawRoll 0 0 0,
      cameraDistance = 1,
      cameraVFov = 45
    }
