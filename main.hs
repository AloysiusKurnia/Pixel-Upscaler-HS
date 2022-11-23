module Main where

import qualified Data.ByteString as Byte
import Src.UpscaleAlgorithm
import Src.Processor
import Src.Color
import Src.Algorithms.NearestNeighbor
import Data.Vector

upscale :: UpscaleType -> Int -> String -> String -> IO ()
upscale algorithm scale inputName outputName =
    Byte.readFile inputName >>= (return . (processWith algorithm scale)) >>= Byte.writeFile outputName

main :: IO ()
main = return ()

exampleImg = Image 5 (Data.Vector.fromList [
  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,

  Color 255 255 255,
  Color 0 255 255,
  Color 255 255 255,
  Color 0 255 255,
  Color 255 255 255,

  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,

  Color 0 255 255,
  Color 0 255 255,
  Color 0 255 255,
  Color 0 255 255,
  Color 0 255 255,

  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255,
  Color 255 255 255])
