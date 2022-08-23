module Main (main) where

import Data.ByteString (pack)
import Graphics.Gloss
  ( BitmapFormat (BitmapFormat),
    Display (InWindow),
    PixelFormat (PxRGBA),
    RowOrder (TopToBottom),
    bitmapOfByteString,
    black,
    display,
  )
import Scene (defaultCamera, defaultScene)
import Tracer (renderScene)
import Utils (colourTo24BitRGBA)

main :: IO ()
main = display window black picture
  where
    render = renderScene defaultScene defaultCamera (screenWidth, screenHeight)
    window = InWindow windowName (screenWidth, screenHeight) (100, 100)
    picture = bitmapOfByteString screenWidth screenHeight (BitmapFormat TopToBottom PxRGBA) bitmapData True
    bitmapData = pack $ concatMap colourTo24BitRGBA (concat render)
    windowName = "funtracer"
    screenWidth = 512
    screenHeight = 512