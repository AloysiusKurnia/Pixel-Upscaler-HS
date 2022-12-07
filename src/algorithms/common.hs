module Src.Algorithms.Common where

import qualified Graphics.Image as Img

-- | If an integer is in the form of 2^m 3^n, return Just (m, n).
-- Otherwise, return Nothing.
factorizeTwoThree :: Int -> Maybe (Int, Int)
factorizeTwoThree n
    | n `mod` 2 == 0 = factorizeTwoThree (n `div` 2) >>= (\(x, y) -> Just (x + 1, y))
    | n `mod` 3 == 0 = factorizeTwoThree (n `div` 3) >>= (\(x, y) -> Just (x, y + 1))
    | n == 1 = Just (0, 0)
    | otherwise = Nothing

-- | A shorhand for RGB Image
type RGBImage = Img.Image Img.VU Img.RGB Double

-- | A shorthand for RGB Pixel
type RGBPixel = Img.Pixel Img.RGB Double

-- | A shorthand for a 2x2 square of any type a.
-- It is ordered top left to top right, bottom left to bottom right.
type Square2x2 a = (
    a, a,
    a, a)

-- | A shorthand for a 3x3 square of any type a.
-- It is ordered top left to top right, middle left to middle right, 
-- bottom left to bottom right.
type Square3x3 a = (
    a, a, a,
    a, a, a,
    a, a, a)

-- | Returns the pixel at the given coordinates. If the coordinates are out of
-- bounds, returns the pixel at the closest coordinates.
safeGetPixel :: RGBImage -> (Int, Int) -> RGBPixel
safeGetPixel img (x, y)
    | x < 0 = Img.index img (0, y)
    | y < 0 = Img.index img (x, 0)
    | x >= Img.cols img = Img.index img (Img.cols img - 1, y)
    | y >= Img.rows img = Img.index img (x, Img.rows img - 1)
    | otherwise = Img.index img (x, y)

-- | Returns the 3x3 neighborhood of the given pixel.
getNeighborhood :: RGBImage -> (Int, Int) -> Square3x3 RGBPixel
getNeighborhood img (x, y) = (
    safeGetPixel img (x - 1, y - 1),
    safeGetPixel img (x, y - 1),
    safeGetPixel img (x + 1, y - 1),

    safeGetPixel img (x - 1, y),
    safeGetPixel img (x, y),
    safeGetPixel img (x + 1, y),

    safeGetPixel img (x - 1, y + 1),
    safeGetPixel img (x, y + 1),
    safeGetPixel img (x + 1, y + 1))

-- | Merges an array of 2x2 squares into a rectangle with height of 2,
-- represented by a tuple containing two arrays of the same length.
stack2x2SquaresHorizontally :: [Square2x2 a] -> ([a], [a])
stack2x2SquaresHorizontally [] = ([], [])
stack2x2SquaresHorizontally ((a, b, c, d):xs) = (a:b:fst rest, c:d:snd rest)
    where rest = stack2x2SquaresHorizontally xs

-- | Merges an array of 3x3 squares into a rectangle with height of 3,
-- represented by a tuple containing three arrays of the same length.
stack3x3SquaresHorizontally :: [Square3x3 a] -> ([a], [a], [a])
stack3x3SquaresHorizontally [] = ([], [], [])
stack3x3SquaresHorizontally ((a, b, c, d, e, f, g, h, i):xs) = (a:b:c:fst3 rest, d:e:f:snd3 rest, g:h:i:thd3 rest)
    where
        fst3 (x, _, _) = x
        snd3 (_, x, _) = x
        thd3 (_, _, x) = x
        rest = stack3x3SquaresHorizontally xs

-- | Merges multiple rectangles with the height of 2 into a 2-dimensional array.
stack2x2RowsVertically :: [([a], [a])] -> [[a]]
stack2x2RowsVertically [] = []
stack2x2RowsVertically ((a, b):xs) = a:b:stack2x2RowsVertically xs

-- | Merges multiple rectangles with the height of 3 into a 2-dimensional array.
stack3x3RowsVertically :: [([a], [a], [a])] -> [[a]]
stack3x3RowsVertically [] = []
stack3x3RowsVertically ((a, b, c):xs) = a:b:c:stack3x3RowsVertically xs

-- | Creates a coordinate lattice of the given size.
xySpace :: Int -> Int -> [[(Int, Int)]]
xySpace width height = [[(x, y) | x <- [0..width - 1]] | y <- [0..height - 1]]

-- | Applies a function returning a square to each element of a 2-dimensional array,
-- expanding the array twice in width and height.
apply2x2 :: (a -> Square2x2 b) -> [[a]] -> [[b]]
apply2x2 f = stack2x2RowsVertically . map (stack2x2SquaresHorizontally . map f)

-- | Applies a function returning a square to each element of a 2-dimensional array,
-- expanding the array three times in width and height.
apply3x3 :: (a -> Square3x3 b) -> [[a]] -> [[b]]
apply3x3 f = stack3x3RowsVertically . map (stack3x3SquaresHorizontally . map f)

-- | Upscales an image by a factor of 2, using the given upsampling function.
upscale2x :: (RGBImage -> (Int, Int) -> Square2x2 RGBPixel) -> RGBImage -> RGBImage
upscale2x algorithm img = Img.transpose 
    $ Img.fromLists 
    $ apply2x2 (algorithm img) 
    $ xySpace (Img.cols img) (Img.rows img)

-- | Upscales an image by a factor of 3, using the given upsampling function.
upscale3x :: (RGBImage -> (Int, Int) -> Square3x3 RGBPixel) -> RGBImage -> RGBImage
upscale3x algorithm img = Img.transpose 
    $ Img.fromLists 
    $ apply3x3 (algorithm img) 
    $ xySpace (Img.cols img) (Img.rows img)

-- | convert rgb color to yuv color
rgb2yuv :: RGBPixel -> (Double, Double, Double)
rgb2yuv (Img.PixelRGB r g b) = (y, u, v)
    where
        y = 0.299 * r + 0.587 * g + 0.114 * b
        u = -0.14713 * r - 0.28886 * g + 0.436 * b
        v = 0.615 * r - 0.51499 * g - 0.10001 * b

-- | count the difference of two yuv colors
yuvDiff :: (Double, Double, Double) -> (Double, Double, Double) -> Bool
yuvDiff (y1, u1, v1) (y2, u2, v2) = abs (y1 - y2) > 3145728 || abs (u1 - u2) > 1792 || abs (v1 - v2) > 6

-- | determine which two color is different enough
isDifferent :: RGBPixel -> RGBPixel -> Bool
isDifferent p1 p2 = yuvDiff (rgb2yuv p1) (rgb2yuv p2)
