module Main where

import qualified Graphics.Image as Img
import Src.Algorithms.HQXPatterns.HQ3X
import Src.Algorithms.HQX
import Src.Algorithms.Common

main :: IO ()
main = Img.readImageRGB Img.VU "example.png" >>=
    return . upscale3x hq3x >>= 
    Img.writeImage "output.png"