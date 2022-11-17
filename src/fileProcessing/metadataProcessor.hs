module Src.FileProcessing.MetadataProcessor where

import Src.FileProcessing.Validator
import Src.FileProcessing.ByteReader

import Data.Bits (popCount, countLeadingZeros)
import Data.ByteString (ByteString)

-- The metadata of a bitmap file. This is the information that is stored in 
-- the header of the file. Some things are assumed:
--   * Alpha channel is always 0xFF,
--   * bits per pixel is 16, 24, or 32,
--   * each colors use the same number of bits (aside from alpha),
--   * no compression is used.
data Metadata = Metadata {
    width :: Int, -- Width of the image in pixels.
    height :: Int, -- Height of the image in pixels.
    offset :: Int, -- Byte offset to the pixel data.
    bitsPerPixel :: Int, -- Number of bits per pixel.
    bitsPerColor :: Int, -- Number of bits per color channel.
    redOffset :: Int, -- Offset of the red channel in bits.
    greenOffset :: Int, -- Offset of the green channel in bits.
    blueOffset :: Int -- Offset of the blue channel in bits.
} deriving (Show)

-- Returns the position of the first occurence of a pixel in the given bytestring.
pixelArrayStart :: ByteString -> Int
pixelArrayStart bytestring = fromIntegral $ read4Bytes bytestring 0xA


-- Returns the size of the image in width and height tuple.
dimensions :: ByteString -> (Int, Int)
dimensions bytestring = (imageWidth bytestring, imageHeight bytestring) where
  imageWidth bytestring = fromIntegral $ read4Bytes bytestring 0x12
  imageHeight bytestring = fromIntegral $ read4Bytes bytestring 0x16


-- Extracts bits per color, red offset, green offset, blue offset from the given bytestring.
extractMasks :: ByteString -> Either String (Int, Int, Int, Int)
extractMasks bytestring = if popCount redMask /= popCount greenMask || popCount redMask /= popCount blueMask
  then Left "The number of bits per color is not the same."
  else Right (bitsPerColor, redOffset, greenOffset, blueOffset) where
    redMask = read4Bytes bytestring 0x36
    greenMask = read4Bytes bytestring 0x3A
    blueMask = read4Bytes bytestring 0x3E

    bitsPerColor = popCount redMask
    redOffset = countLeadingZeros redMask
    greenOffset = countLeadingZeros greenMask
    blueOffset = countLeadingZeros blueMask


-- Returns the metadata of the given bitmap file.
getMetadata :: ByteString -> Either String Metadata
getMetadata bytestring = do
  validateProgramLength bytestring
  validateIsBmp bytestring
  validateNoCompression bytestring
  validateHeaderSize bytestring

  bpp <- extractBitsPerPixel bytestring
  (bitsPerColor, redOffset, greenOffset, blueOffset) <- extractMasks bytestring
  (width, height) <- return $ dimensions bytestring

  return Metadata {
    width = width,
    height = height,
    offset = pixelArrayStart bytestring,
    bitsPerPixel = bpp,
    bitsPerColor = bitsPerColor,
    redOffset = redOffset,
    greenOffset = greenOffset,
    blueOffset = blueOffset
  }
