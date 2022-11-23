module Src.UpscaleAlgorithm where
import Src.Color
import Data.Vector

data UpscaleType = NearestNeighbor | ScaleX | HQX

data Image = Image { width :: Int, pixels :: Vector Color } deriving Show

height :: Image -> Int
height image = (Data.Vector.length (pixels image)) `div` (width image)

getColor:: Image -> Int -> Int -> Color
getColor image x y = pixels image ! (y * width image + x)