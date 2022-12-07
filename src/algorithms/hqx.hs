module Src.Algorithms.HQX where

import Src.Algorithms.HQXPatterns.HQ2X
import Src.Algorithms.HQXPatterns.HQ3X
import Src.Algorithms.Common

hq2x :: RGBImage -> (Int, Int) -> Square2x2 RGBPixel
hq2x img (x, y) = getPixelValueHQ2x (getNeighborhood img (x, y))

hq3x :: RGBImage -> (Int, Int) -> Square3x3 RGBPixel
hq3x img (x, y) = getPixelValueHQ3x (getNeighborhood img (x, y))