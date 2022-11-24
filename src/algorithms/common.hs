module Src.Algorithms.Common where

import qualified Graphics.Image as Img

-- | If an integer is in the form of 2^m 3^n, return Just (m, n).
-- | Otherwise, return Nothing.
factorizeTwoThree :: Int -> Maybe (Int, Int)
factorizeTwoThree n
    | n `mod` 2 == 0 = factorizeTwoThree (n `div` 2) >>= (\(x, y) -> Just (x + 1, y))
    | n `mod` 3 == 0 = factorizeTwoThree (n `div` 3) >>= (\(x, y) -> Just (x, y + 1))
    | n == 1 = Just (0, 0)
    | otherwise = Nothing

type RGBImage = Img.Image Img.VU Img.RGB Double
type RGBPixel = Img.Pixel Img.RGB Double
type Square2x2 a = (
    a, a,
    a, a)
type Square3x3 a = (
    a, a, a,
    a, a, a,
    a, a, a)

safeGetPixel :: RGBImage -> (Int, Int) -> RGBPixel
safeGetPixel img (x, y)
    | x < 0 = Img.index img (0, y)
    | y < 0 = Img.index img (x, 0)
    | x >= Img.cols img = Img.index img (Img.cols img - 1, y)
    | y >= Img.rows img = Img.index img (x, Img.rows img - 1)
    | otherwise = Img.index img (x, y)

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

stack2x2SquaresHorizontally :: [Square2x2 a] -> ([a], [a])
stack2x2SquaresHorizontally [] = ([], [])
stack2x2SquaresHorizontally ((a, b, c, d):xs) = (a:b:fst rest, c:d:snd rest)
    where rest = stack2x2SquaresHorizontally xs

stack3x3SquaresHorizontally :: [Square3x3 a] -> ([a], [a], [a])
stack3x3SquaresHorizontally [] = ([], [], [])
stack3x3SquaresHorizontally ((a, b, c, d, e, f, g, h, i):xs) = (a:b:c:fst3 rest, d:e:f:snd3 rest, g:h:i:thd3 rest)
    where
        fst3 (x, _, _) = x
        snd3 (_, x, _) = x
        thd3 (_, _, x) = x
        rest = stack3x3SquaresHorizontally xs

stack2x2RowsVertically :: [([a], [a])] -> [[a]]
stack2x2RowsVertically [] = []
stack2x2RowsVertically ((a, b):xs) = a:b:stack2x2RowsVertically xs

stack3x3RowsVertically :: [([a], [a], [a])] -> [[a]]
stack3x3RowsVertically [] = []
stack3x3RowsVertically ((a, b, c):xs) = a:b:c:stack3x3RowsVertically xs

xySpace :: Int -> Int -> [[(Int, Int)]]
xySpace width height = [[(x, y) | x <- [0..width - 1]] | y <- [0..height - 1]]

apply2x2 :: (a -> Square2x2 b) -> [[a]] -> [[b]]
apply2x2 f = stack2x2RowsVertically . map (stack2x2SquaresHorizontally . map f)

apply3x3 :: (a -> Square3x3 b) -> [[a]] -> [[b]]
apply3x3 f = stack3x3RowsVertically . map (stack3x3SquaresHorizontally . map f)

upscale2x :: (RGBImage -> (Int, Int) -> Square2x2 RGBPixel) -> RGBImage -> RGBImage
upscale2x algorithm img = Img.transpose 
    $ Img.fromLists 
    $ apply2x2 (algorithm img) 
    $ xySpace (Img.cols img) (Img.rows img)