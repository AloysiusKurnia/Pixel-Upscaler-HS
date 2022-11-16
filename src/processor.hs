module Src.Processor where

import qualified Data.ByteString as Byte
import Src.UpscaleAlgorithm

processWith :: UpscaleType -> Int -> Byte.ByteString -> Byte.ByteString
processWith _ _ x = x