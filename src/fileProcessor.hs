module Src.FileProcessor where

import qualified Data.ByteString as Byte

-- | Given a byte string, return the number given by byte offset and byte size.
readLittleEndian :: Byte.ByteString -> Int -> Int -> Integer
readLittleEndian bytestring at 1 = toInteger $ Byte.index bytestring at
readLittleEndian bytestring at n = (toInteger $ Byte.index bytestring at) + (readLittleEndian bytestring (at + 1) (n - 1)) * 0x100

-- | Returns the position of the first occurence of a pixel in the given bytestring.
pixelArrayStart :: Byte.ByteString -> Int
pixelArrayStart bytestring = fromIntegral $ readLittleEndian bytestring 0xA 4

-- | Returns the width of the image in pixels
imageWidth :: Byte.ByteString -> Int
imageWidth bytestring = fromIntegral $ readLittleEndian bytestring 0x12 4

-- | Returns the height of the image in pixels
imageHeight :: Byte.ByteString -> Int
imageHeight bytestring = fromIntegral $ readLittleEndian bytestring 0x16 4

-- | Obtain the red, green, and blue bitmasks from the header.
bitmasks :: Byte.ByteString -> (Integer, Integer, Integer)
bitmasks bytestring = (
  readLittleEndian bytestring 0x36 4,
  readLittleEndian bytestring 0x3A 4,
  readLittleEndian bytestring 0x3E 4)

-- | Verify that the file is a valid bitmap file with supported headers and bits per pixel.
verifyBitmap :: Byte.ByteString -> Bool
verifyBitmap bytestring = isBmp &&
  (  bitsPerPixel == 16
  || bitsPerPixel == 24
  || bitsPerPixel == 32) && 
  (  headerIndicatorByte == 52 -- BITMAPV2HEADER
  || headerIndicatorByte == 56 -- BITMAPV3HEADER
  || headerIndicatorByte == 108 -- BITMAPV4HEADER
  || headerIndicatorByte == 124 -- BITMAPV5HEADER
  ) where isBmp = readLittleEndian bytestring 0 2 == 0x4D42
          bitsPerPixel = readLittleEndian bytestring 0x1C 2
          headerIndicatorByte = readLittleEndian bytestring 0xE 4
