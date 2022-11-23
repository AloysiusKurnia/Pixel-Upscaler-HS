module Src.UpscaleAlgorithm where
import Src.Color
import Vector

data UpscaleType = NearestNeighbor | ScaleX | HQX

data Image = Image { width :: Int, pixels :: Vector Color }

getColor:: Image -> Int -> Int -> Color
getColor image x y = pixels image ! (y * width image + x)