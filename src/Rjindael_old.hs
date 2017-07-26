{-# LANGUAGE OverloadedStrings #-}
module Rjindael where

import qualified Data.ByteString as BS
import qualified Data.Bits as B
import GaloisFields
import Data.Bits ((.&.),(.|.))
import Numeric (showHex,showIntAtBase)
import Data.Char (intToDigit)
import Data.Word
import Data.Bits.ByteString
import qualified Data.Matrix as Mat

import Utils

nb = 4 :: Int
nk = 4 :: Int
nr_128 = 10 :: Int
nr_192 = 12 :: Int
nr_256 = 14 :: Int

multParts :: Int -> Int -> Int -> [Int]
multParts n a b 
  | n > a     = []
  | otherwise = (n .&. a) * b : multParts (B.shiftL n 1) a b

rotateByteStringL :: Int -> BS.ByteString -> BS.ByteString
rotateByteStringL i bs = BS.append (BS.drop i bs) (BS.take i bs)

rconMod :: BS.ByteString
rconMod = BS.pack $ [1,27]

rconModInt :: Int
rconModInt = 0x11b

testin = "abcdefghijklmnop" :: BS.ByteString

inputToStateMatrix :: BS.ByteString -> Mat.Matrix Word8
inputToStateMatrix bs = Mat.fromList 4 4 $ BS.unpack bs

stateMatrixToOutput :: Mat.Matrix Word8 -> BS.ByteString
stateMatrixToOutput mat = BS.pack $ Mat.toList mat



mult4Poly :: [Word8] -> [Word8] -> [Word8]
mult4Poly [a3,a2,a1,a0] [b3,b2,b1,b0] = [d3,d2,d1,d0]
  where
    d0 = (multGF2 aes_mod a0 b0) `addGF2` (multGF2 aes_mod a3 b1) `addGF2` (multGF2 aes_mod a2 b2) `addGF2` (multGF2 aes_mod a1 b3)
    d1 = (multGF2 aes_mod a1 b0) `addGF2` (multGF2 aes_mod a0 b1) `addGF2` (multGF2 aes_mod a3 b2) `addGF2` (multGF2 aes_mod a2 b3)
    d2 = (multGF2 aes_mod a2 b0) `addGF2` (multGF2 aes_mod a1 b1) `addGF2` (multGF2 aes_mod a0 b2) `addGF2` (multGF2 aes_mod a3 b3)
    d3 = (multGF2 aes_mod a3 b0) `addGF2` (multGF2 aes_mod a1 b1) `addGF2` (multGF2 aes_mod a1 b2) `addGF2` (multGF2 aes_mod a0 b3)
