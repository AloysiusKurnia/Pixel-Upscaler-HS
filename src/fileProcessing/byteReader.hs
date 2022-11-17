module Src.FileProcessor.ByteReader where

import qualified Data.ByteString as Byte
import qualified Data.Bits as Bits

-- Reads n bytes from the given bytestring starting at the given offset.
-- The bytes are read in little endian order.
readByte :: Byte.ByteString -> Int -> Int -> Integer
readByte bytestring at 1 = toInteger $ Byte.index bytestring at
readByte bytestring at n = (toInteger $ Byte.index bytestring at) + (readLittleEndian bytestring (at + 1) (n - 1)) * 0x100

-- Extracts how many bits are used for each color channel.
extractBitSizeFromMask :: Word32 -> Int
extractBitSizeFromMask mask = Bits.popCount mask