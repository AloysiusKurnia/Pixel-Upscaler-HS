module Src.Color where
-- create data structure for color
data Color = Color { red :: Int, green :: Int, blue :: Int } deriving (Show)

-- extracting rgb values from a color
getRed :: Color -> Int
getRed (Color r _ _) = r

getGreen :: Color -> Int
getGreen (Color _ g _) = g

getBlue :: Color -> Int
getBlue (Color _ _ b) = b

-- weighted sum of two colors
weightedSum :: Color -> Color -> Int -> Int -> Color
weightedSum (Color r1 g1 b1) (Color r2 g2 b2) w1 w2 = Color (weightedSum' r1 r2 w1 w2) (weightedSum' g1 g2 w1 w2) (weightedSum' b1 b2 w1 w2)

weightedSum' :: Int -> Int -> Int -> Int -> Int
weightedSum' a b w1 w2 = (a * w1 + b * w2) `div` (w1 + w2)

-- weighted sum of three colors
weightedSum3 :: Color -> Color -> Color -> Int -> Int -> Int -> Color
weightedSum3 (Color r1 g1 b1) (Color r2 g2 b2) (Color r3 g3 b3) w1 w2 w3 = 
    Color (weightedSum3' r1 r2 r3 w1 w2 w3) (weightedSum3' g1 g2 g3 w1 w2 w3) (weightedSum3' b1 b2 b3 w1 w2 w3)

weightedSum3' :: Int -> Int -> Int -> Int -> Int -> Int -> Int
weightedSum3' a b c w1 w2 w3 = (a * w1 + b * w2 + c * w3) `div` (w1 + w2 + w3)

-- convert color to YUV
toYUV :: Color -> Color
toYUV (Color r g b) = Color y u v
    where
        y = (r * 299 + g * 587 + b * 114) `div` 1000
        u = (b * 564 - g * 534 - r * 30) `div` 1000 + 128
        v = (r * 713 - g * 642 - b * 71) `div` 1000 + 128
