module Src.Interface where

import qualified Graphics.Image as Img
import Src.Algorithms.Common
import Src.Algorithms.ScaleX
import Src.Algorithms.HQX
import Src.Algorithms.NearestNeighbor

data Algorithm = ScaleX | NearestNeigbor | HQX

upscaleAlgorithm2 :: Algorithm -> (RGBImage -> RGBImage)
upscaleAlgorithm2 ScaleX = upscale2x scale2x
upscaleAlgorithm2 NearestNeigbor = upscale2x nearestNeighbor2x
upscaleAlgorithm2 HQX = upscale2x hq2x

upscaleAlgorithm3 :: Algorithm -> (RGBImage -> RGBImage)
upscaleAlgorithm3 ScaleX = upscale3x scale3x
upscaleAlgorithm3 NearestNeigbor = upscale3x nearestNeighbor3x
upscaleAlgorithm3 HQX = upscale3x hq3x

upscaleFactorized :: Algorithm -> (Int, Int) -> RGBImage -> RGBImage
upscaleFactorized algorithm (0, 0) img = img
upscaleFactorized algorithm (n, 0) img = upscaleFactorized algorithm (n - 1, 0) ((upscaleAlgorithm2 algorithm) img)
upscaleFactorized algorithm (p, n) img = upscaleFactorized algorithm (p, n - 1) ((upscaleAlgorithm3 algorithm) img)

upscale :: Algorithm -> Int -> RGBImage -> Maybe RGBImage
upscale algorithm n img = factorizeTwoThree n >>= \factor -> return $ upscaleFactorized algorithm factor img

writeIfValid :: Maybe RGBImage -> String -> IO ()
writeIfValid Nothing _ = putStrLn "Scaling factor is not in the form of 2^m * 3^n. Upscaling failed."
writeIfValid (Just img) filename = Img.writeImage filename img