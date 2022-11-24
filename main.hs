module Main where

import qualified Graphics.Image as Img
import Src.Algorithms.ScaleX
import Src.Algorithms.Common
-- upscale :: UpscaleType -> Int -> String -> String -> IO ()
-- upscale algorithm scale inputName outputName =
--     Byte.readFile inputName >>= (return $ processWith algorithm scale) >>= Byte.writeFile outputName

main :: IO ()
-- main = upscale NearestNeighbor 2 "example.bmp" "output.bmp"
main = Img.readImageRGB Img.VU "example.png" >>=
    return . upscale2x scale2x >>= 
    Img.writeImage "output.png"