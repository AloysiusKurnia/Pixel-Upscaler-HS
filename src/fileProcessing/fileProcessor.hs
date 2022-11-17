module Src.FileProcessor where

import Src.FileProcessor.Validator
import Src.FileProcessor.ByteReader

-- The metadata of a bitmap file. This is the information that is stored in 
-- the header of the file. Some things are assumed:
--   * Alpha channel is always 0xFF,
--   * bits per pixel is 16, 24, or 32,
--   * each colors use the same number of bits (aside from alpha),
--   * no compression is used.
data Metadata = Metadata {
    width :: Int,
    height :: Int,
    offset :: Int,
    bitsPerPixel :: Int,
    bitsPerColor :: Int,
    redOffset :: Int,
    greenOffset :: Int,
    blueOffset :: Int
} deriving (Show)


-- Returns the position of the first occurence of a pixel in the given bytestring.
pixelArrayStart :: Byte.ByteString -> Int
pixelArrayStart bytestring = fromIntegral $ readLittleEndian bytestring 0xA 4

-- Returns the size of the image in width and height tuple.
dimensions :: Byte.ByteString -> (Int, Int)
dimensions bytestring = (imageWidth bytestring, imageHeight bytestring) where
  imageWidth bytestring = fromIntegral $ readLittleEndian bytestring 0x12 4
  imageHeight bytestring = fromIntegral $ readLittleEndian bytestring 0x16 4

-- Obtain the red, green, and blue bitmasks from the header.
bitmasks :: Byte.ByteString -> (Integer, Integer, Integer)
bitmasks bytestring = (
  readLittleEndian bytestring 0x36 4,
  readLittleEndian bytestring 0x3A 4,
  readLittleEndian bytestring 0x3E 4)
