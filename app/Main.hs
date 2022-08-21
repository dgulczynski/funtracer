module Main (main) where

import Graphics.Gloss (Display (InWindow), display, white)
import Tracer (renderScene)

main :: IO ()
main = display window white render
  where
    render = renderScene (screenWidth, screenHeight)
    window = InWindow windowName (screenWidth, screenHeight) (screenWidth, screenHeight)
    windowName = "funtracer"
    screenWidth = 256
    screenHeight = 256