module Main where

import qualified Graphics.Image as Img
import Src.Algorithms.HQXPatterns.HQ2X
import Src.Algorithms.HQX
import Src.Algorithms.Common

main :: IO ()
main = Img.readImageRGB Img.VU "example.png" >>=
    return . upscale2x hq2x >>= 
    Img.writeImage "output.png"