module Src.Algorithms.HQX where

import qualified Graphics.Image as Img
import Src.Algorithms.Common

----- HQX Interpolation patterns -----

interpHQX :: Int -> RGBPixel -> RGBPixel -> RGBPixel -> RGBPixel
interpHQX 1 c1 c2 _ = (c1 * 3 + c2) / 4
interpHQX 2 c1 c2 c3 = (c1 * 2 + c2 + c3) / 4
interpHQX 3 c1 c2 _ = (c1 * 7 + c2) / 8
interpHQX 4 c1 c2 c3 = (c1 * 2 + (c2 + c3) * 7)/16
interpHQX 5 c1 c2 _ = (c1 + c2) / 2
interpHQX 6 c1 c2 c3 = (c1 * 5 + c2 * 2 + c3) / 8
interpHQX 7 c1 c2 c3 = (c1 * 6 + c2 + c3) / 8
interpHQX 8 c1 c2 _ = (c1 * 5 + c2 * 3) / 8
interpHQX 9 c1 c2 c3 = (c1 * 2 + c2 * 3 + c3 * 3) / 8
interpHQX 10 c1 c2 c3 = (c1 * 14 + c2 + c3) / 16

getNthColorFromNeighborhood :: Int -> Square3x3 RGBPixel -> RGBPixel
getNthColorFromNeighborhood 1 (c1, _, _, _, _, _, _, _, _) = c1
getNthColorFromNeighborhood 2 (_, c2, _, _, _, _, _, _, _) = c2
getNthColorFromNeighborhood 3 (_, _, c3, _, _, _, _, _, _) = c3
getNthColorFromNeighborhood 4 (_, _, _, c4, _, _, _, _, _) = c4
getNthColorFromNeighborhood 5 (_, _, _, _, c5, _, _, _, _) = c5
getNthColorFromNeighborhood 6 (_, _, _, _, _, c6, _, _, _) = c6
getNthColorFromNeighborhood 7 (_, _, _, _, _, _, c7, _, _) = c7
getNthColorFromNeighborhood 8 (_, _, _, _, _, _, _, c8, _) = c8
getNthColorFromNeighborhood 9 (_, _, _, _, _, _, _, _, c9) = c9

blendHQX :: Square3x3 RGBPixel -> Int -> Int -> Int -> Int -> RGBPixel
blendHQX neighborhood pattern c1 c2 c3 = interpHQX pattern 
    (getNthColorFromNeighborhood c1 neighborhood) 
    (getNthColorFromNeighborhood c2 neighborhood) 
    (getNthColorFromNeighborhood c3 neighborhood)