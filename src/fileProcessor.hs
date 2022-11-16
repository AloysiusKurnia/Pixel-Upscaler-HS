module Src.FileProcessor where

import qualified Data.ByteString as Byte

readLittleEndian :: Byte.ByteString -> Int -> Int -> Integer
readLittleEndian bytestring at 1 = toInteger $ Byte.index bytestring at
readLittleEndian bytestring at n = (toInteger $ Byte.index bytestring at) + (readLittleEndian bytestring (at + 1) (n - 1)) * 256

-- Returns the position of the first occurence of a pixel in the given bytestring
pixelArrayStart :: Byte.ByteString -> Int
pixelArrayStart bytestring = fromIntegral $ readLittleEndian bytestring 10 4

-- Returns the width of the image in pixels
imageWidth :: Byte.ByteString -> Int
imageWidth bytestring = fromIntegral $ readLittleEndian bytestring 18 4

-- Returns the height of the image in pixels
imageHeight :: Byte.ByteString -> Int
imageHeight bytestring = fromIntegral $ readLittleEndian bytestring 22 4

-- Returns the number of bytes per pixel
bytesPerPixel :: Byte.ByteString -> Int
bytesPerPixel bytestring = fromIntegral $ readLittleEndian bytestring 28 2

-- Obtain the red, green, and blue bitmasks from the header
bitmasks :: Byte.ByteString -> (Integer, Integer, Integer)
bitmasks bytestring = (readLittleEndian bytestring 54 4, readLittleEndian bytestring 58 4, readLittleEndian bytestring 62 4)

-- Verify that the file is a valid bitmap file with supported headers
verifyBitmap :: Byte.ByteString -> Bool
verifyBitmap bytestring = (byteAt 0 == 66) && (byteAt 1 == 77) &&
  (  byteAt14 == 40 -- BITMAPV2HEADER
  || byteAt14 == 52 -- BITMAPV3HEADER
  || byteAt14 == 56 -- BITMAPV4HEADER
  || byteAt14 == 108 -- BITMAPV5HEADER
  ) where byteAt14 = byteAt 14
          byteAt = Byte.index bytestring
