module Src.FileProcessing.ByteReader where

import Data.ByteString (ByteString, index)
import Data.Word (Word16, Word32)
import Data.Bits (shiftL, shiftR, (.|.))

-- Returns two bytes from the given bytestring, starting at the given index.
-- The bytes are read in little endian, unsigned format.
read2Bytes :: ByteString -> Int -> Word16
read2Bytes bytestring at = (fromIntegral $ index bytestring (at))
                       .|. (fromIntegral $ index bytestring (at + 1)) `shiftL` 8

-- Returns four bytes from the given bytestring, starting at the given index.
-- The bytes are read in little endian, unsigned format.
read4Bytes :: ByteString -> Int -> Word32
read4Bytes bytestring at = (fromIntegral $ index bytestring (at))
                       .|. (fromIntegral $ index bytestring (at + 1)) `shiftL` 8
                       .|. (fromIntegral $ index bytestring (at + 2)) `shiftL` 16
                       .|. (fromIntegral $ index bytestring (at + 3)) `shiftL` 24