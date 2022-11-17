module Src.FileProcessor.Validator where

validateProgramLength :: Byte.ByteString -> Either String ()
validateProgramLength bytestring = if programLength < 0x36
  then Left "File is too short to be a valid bitmap file."
  else Right () where
    programLength = Byte.length bytestring

validateNoCompression :: Byte.ByteString -> Either String ()
validateNoCompression bytestring = if compression /= 0
  then Left "Compression is not supported."
  else Right () where
    compression = readLittleEndian bytestring 0x1E 4

validateIsBmp :: Byte.ByteString -> Either String ()
validateIsBmp bytestring = if 
    (readByte bytestring 0x0 2 == 0x4D42) -- BM
    then Right ()
    else Left "This is not a BMP file."

validateHeaderSize :: Byte.ByteString -> Either String ()
validateHeaderSize bytestring = if 
    (headerSize == 52 -- BITMAPV2INFOHEADER
  || headerSize == 56 -- BITMAPV3INFOHEADER
  || headerSize == 108 -- BITMAPV4HEADER
  || headerSize == 124) -- BITMAPV5HEADER
    then Right ()
    else Left "Unsupported header size. This program only supports BITMAPV2INFOHEADER, BITMAPV3INFOHEADER, BITMAPV4HEADER, and BITMAPV5HEADER." where
      headerSize = fromIntegral $ readByte bytestring 0xE 4

bitsPerPixel :: Byte.ByteString -> Either String Int
bitsPerPixel bytestring = if bpp == 16 || bpp == 24 || bpp == 32 
  then Right bpp
  else Left "Unsupported bits per pixel" where
    bpp = fromIntegral $ readByte bytestring 0x1C 2