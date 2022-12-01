module Src.Algorithms.NearestNeighbor where

import qualified Graphics.Image as Img
import Src.Algorithms.Common

-- | Given a pixel and its neighbors, scales the pixel twice.
nn2xNeigborhood :: Square3x3 RGBPixel -> Square2x2 RGBPixel
nn2xNeigborhood (_, _, _, _, e, _, _, _, _) = (e, e, e, e)

-- | Given a pixel and its neighbors, scales the pixel three times.
nn3xNeigborhood :: Square3x3 RGBPixel -> Square3x3 RGBPixel
nn3xNeigborhood (_, _, _, _, e, _, _, _, _) = (e, e, e, e, e, e, e, e, e)

-- | The nearest neighbor algorithm to scale an image 2x.
nearestNeighbor2x :: RGBImage -> (Int, Int) -> Square2x2 RGBPixel
nearestNeighbor2x img (x, y) = nn2xNeigborhood (getNeighborhood img (x, y))

-- | The nearest neighbor algorithm to scale an image 3x.
nearestNeighbor3x :: RGBImage -> (Int, Int) -> Square3x3 RGBPixel
nearestNeighbor3x img (x, y) = nn3xNeigborhood (getNeighborhood img (x, y))