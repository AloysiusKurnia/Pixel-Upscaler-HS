module Src.FileProcessor where

import qualified Data.ByteString as Byte

data Metadata = Metadata {
    width :: Int,
    height :: Int,
    offset :: Int,
    bitsPerPixel :: Int,
    bitsPerColor :: Int
} deriving (Show)

-- | Given a byte string, return the number given by byte offset and byte size.
readLittleEndian :: Byte.ByteString -> Int -> Int -> Integer
readLittleEndian bytestring at 1 = toInteger $ Byte.index bytestring at
readLittleEndian bytestring at n = (toInteger $ Byte.index bytestring at) + (readLittleEndian bytestring (at + 1) (n - 1)) * 0x100

-- | Returns the position of the first occurence of a pixel in the given bytestring.
pixelArrayStart :: Byte.ByteString -> Int
pixelArrayStart bytestring = fromIntegral $ readLittleEndian bytestring 0xA 4

-- | Returns the size of the image in width and height tuple.
dimensions :: Byte.ByteString -> (Int, Int)
dimensions bytestring = (imageWidth bytestring, imageHeight bytestring) where
  imageWidth bytestring = fromIntegral $ readLittleEndian bytestring 0x12 4
  imageHeight bytestring = fromIntegral $ readLittleEndian bytestring 0x16 4

-- | Obtain the red, green, and blue bitmasks from the header.
bitmasks :: Byte.ByteString -> (Integer, Integer, Integer)
bitmasks bytestring = (
  readLittleEndian bytestring 0x36 4,
  readLittleEndian bytestring 0x3A 4,
  readLittleEndian bytestring 0x3E 4)

bitsPerPixel :: Byte.ByteString -> Either String Int
bitsPerPixel bytestring = if bpp == 16 || bpp == 24 || bpp == 32 
  then Right bpp
  else Left "Unsupported bits per pixel" where
    bpp = fromIntegral $ readLittleEndian bytestring 0x1C 2

validateHeaderSize :: Byte.ByteString -> Either String ()
validateHeaderSize bytestring = if 
    (headerSize == 52 -- BITMAPV2INFOHEADER
  || headerSize == 56 -- BITMAPV3INFOHEADER
  || headerSize == 108 -- BITMAPV4HEADER
  || headerSize == 124) -- BITMAPV5HEADER
    then Right ()
    else Left "Unsupported header size. This program only supports BITMAPV2INFOHEADER, BITMAPV3INFOHEADER, BITMAPV4HEADER, and BITMAPV5HEADER." where
      headerSize = fromIntegral $ readLittleEndian bytestring 0xE 4

validateProgramLength :: Byte.ByteString -> Either String ()
validateProgramLength bytestring = if programLength < 0x36
  then Left "File is too short to be a valid bitmap file."
  else Right () where
    programLength = Byte.length bytestring
