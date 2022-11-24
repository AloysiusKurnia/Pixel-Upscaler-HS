module Src.Algorithms.ScaleX where

import qualified Graphics.Image as Img
import Src.Algorithms.Common

-- | Given a pixel and its neighbors, scales the pixel twice.
scale2xNeigborhood :: Square3x3 RGBPixel -> Square2x2 RGBPixel
scale2xNeigborhood (a, b, c, d, e, f, g, h, i)
    | b /= h && d /= f = (
        if b == d then d else e,
        if b == f then f else e,

        if d == h then d else e,
        if f == h then f else e)
    | otherwise = (e, e, e, e)

-- | Given a pixel and its neighbors, scales the pixel three times.
scale3xNeigborhood :: Square3x3 RGBPixel -> Square3x3 RGBPixel
scale3xNeigborhood (a, b, c, d, e, f, g, h, i)
    | b /= h && d /= f = (
        if b == d then d else e,
        if (d == b && e /= c) || (b == f && e /= a) then b else e,
        if b == f then f else e,

        if (d == b && e /= g) || (d == h && e /= a) then d else e,
        e,
        if (b == f && e /= i) || (h == f && e /= c) then f else e,

        if d == h then d else e,
        if (d == h && e /= a) || (h == f && e /= g) then h else e,
        if f == h then f else e)
    | otherwise = (e, e, e, e, e, e, e, e, e)

-- | The scale2x algorithm.
scale2x :: RGBImage -> (Int, Int) -> Square2x2 RGBPixel
scale2x img (x, y) = scale2xNeigborhood (getNeighborhood img (x, y))

-- | The scale3x algorithm.
scale3x :: RGBImage -> (Int, Int) -> Square3x3 RGBPixel
scale3x img (x, y) = scale3xNeigborhood (getNeighborhood img (x, y))