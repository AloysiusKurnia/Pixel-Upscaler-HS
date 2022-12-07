module Main where

import qualified Graphics.Image as Img
import Src.Interface

pixelArtUpscaler :: String -> Algorithm -> Int -> String -> IO ()
pixelArtUpscaler input algorithm n output = Img.readImageRGB Img.VU input
    >>= \img -> writeIfValid (upscale algorithm n img) output

main :: IO ()
main = pixelArtUpscaler "example.png" NearestNeigbor 2 "example-2x.png"