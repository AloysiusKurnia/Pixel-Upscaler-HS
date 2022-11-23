module Src.Algorithms.NearestNeighbor where

import Src.Color
import Vector

-- | Upscales an image using nearest neighbor algorithm
upscaleNearestNeighbor :: Image -> Int -> Image
upscaleNearestNeighbor image scale = Image (width image * scale) (upscaleNearestNeighbor' image scale)

upscaleNearestNeighbor' :: Image -> Int -> Vector Color
upscaleNearestNeighbor' image scale = 
    map (getColor image) (map (nearestNeighborIndex image scale) [0..(width image * height image * scale * scale - 1)])

nearestNeighborIndex :: Image -> Int -> Int -> Int
nearestNeighborIndex image scale index = 
    let
        x = index `mod` (width image * scale)
        y = index `div` (width image * scale)
    in
        (y `div` scale) * width image + (x `div` scale)
