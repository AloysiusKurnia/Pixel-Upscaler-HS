module Src.FileProcessing.Validator where

import Data.ByteString (ByteString, length)
import Src.FileProcessing.ByteReader

validateProgramLength :: ByteString -> Either String ()
validateProgramLength bytestring = if programLength < 0x36
  then Left "File is too short to be a valid bitmap file."
  else Right () where
    programLength = Data.ByteString.length bytestring

validateNoCompression :: ByteString -> Either String ()
validateNoCompression bytestring = if compression /= 0 && compression /= 3
  then Left "Compression is not supported. Supported compression types are BI_RGB and BI_BITFIELDS."
  else Right () where
    compression = read4Bytes bytestring 0x1E
  
validateIsBmp :: ByteString -> Either String ()
validateIsBmp bytestring = if 
    (read2Bytes bytestring 0x0 == 0x4D42) -- BM
    then Right ()
    else Left "This is not a BMP file."

validateHeaderSize :: ByteString -> Either String ()
validateHeaderSize bytestring = if 
    (headerSize == 52 -- BITMAPV2INFOHEADER
  || headerSize == 56 -- BITMAPV3INFOHEADER
  || headerSize == 108 -- BITMAPV4HEADER
  || headerSize == 124) -- BITMAPV5HEADER
    then Right ()
    else Left "Unsupported header size. This program only supports BITMAPV2INFOHEADER, BITMAPV3INFOHEADER, BITMAPV4HEADER, and BITMAPV5HEADER." where
      headerSize = read4Bytes bytestring 0xE

extractBitsPerPixel :: ByteString -> Either String Int
extractBitsPerPixel bytestring = if bpp == 16 || bpp == 24 || bpp == 32 
  then Right bpp
  else Left "Unsupported bits per pixel. This program only supports 16, 24, and 32 bits per pixel." where
    bpp = fromIntegral $ read2Bytes bytestring 0x1C