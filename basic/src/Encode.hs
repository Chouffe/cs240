module Encode where

import Data.Word (Word16)
import Data.Char (ord)
import Data.Bits ((.&.), shiftR)

encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00
