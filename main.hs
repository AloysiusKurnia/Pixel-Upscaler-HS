module Main where

import qualified Graphics.Image as Img
import Src.Algorithms.ScaleX
import Src.Algorithms.Common

main :: IO ()
main = Img.readImageRGB Img.VU "example.png" >>=
    return . upscale3x scale3x >>= 
    Img.writeImage "output.png"