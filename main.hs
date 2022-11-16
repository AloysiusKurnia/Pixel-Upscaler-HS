module Main where

import qualified Data.ByteString as Byte
import Src.UpscaleAlgorithm
import Src.Processor

upscale :: UpscaleType -> Int -> String -> String -> IO ()
upscale algorithm scale inputName outputName =
    Byte.readFile inputName >>= (return . (processWith algorithm scale)) >>= Byte.writeFile outputName

main :: IO ()
main = upscale NearestNeighbor "example.bmp" "output.bmp"
