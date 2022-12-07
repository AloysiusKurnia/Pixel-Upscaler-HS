module Main where

import qualified Graphics.Image as Img
import Src.Algorithms.HQX
import Src.Algorithms.Common

main :: IO ()
main = Img.readImageRGB Img.VU "example.png" >>=
    return . upscale3x hq3x >>= 
    return . upscale2x hq2x >>= 
    Img.writeImage "output.png"