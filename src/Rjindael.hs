{-# LANGUAGE OverloadedStrings #-}
module Rjindael where

import qualified Data.ByteString as BS
import qualified Data.Bits as B
import GaloisFields
import Data.Bits ((.&.),(.|.),xor)
import Numeric (showHex,showIntAtBase)
import Data.Char (intToDigit)
import Data.Word
import Data.Bits.ByteString
import Data.List.Split (chunksOf)
import qualified Data.Matrix as Mat
import Data.HexString

import Utils
import KeyExpansion (expandKey)

nb = 4 :: Int
nk_128 = 4 :: Int
nk_192 = 6 :: Int
nk_256 = 8 :: Int
nr_128 = 10 :: Int
nr_192 = 12 :: Int
nr_256 = 14 :: Int

aes_mod = aes_GF2_mod

          
gf2_8_a = [3,1,1,2]

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

inputToStateMatrix :: BS.ByteString -> Mat.Matrix Word8
inputToStateMatrix bs = Mat.transpose . Mat.fromList 4 4 $ BS.unpack bs

stateMatrixToOutput :: Mat.Matrix Word8 -> BS.ByteString
stateMatrixToOutput mat = BS.pack . Mat.toList 
                        . Mat.transpose $ mat


hexMatrix :: Mat.Matrix Word8 -> Mat.Matrix String
hexMatrix = fmap (\a -> showHex a "")

              
cipher128 :: BS.ByteString -> BS.ByteString -> BS.ByteString
cipher128 key a = stateMatrixToOutput . mxor rf 
                . subBytes . shiftRows 
                . foldl cipherRound initState $ rs
  where
    initState = mxor (inputToStateMatrix a) r0
    roundKeys = expandKey nk_128 nb nr_128 (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr_128-1) . tail $ roundKeys
    rf = roundKeys !! nr_128

cipher192 :: BS.ByteString -> BS.ByteString -> BS.ByteString
cipher192 key a = stateMatrixToOutput . mxor rf 
                . subBytes . shiftRows 
                . foldl cipherRound initState $ rs
  where
    initState = mxor (inputToStateMatrix a) r0
    roundKeys = expandKey nk_192 nb nr_192 (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr_192-1) . tail $ roundKeys
    rf = roundKeys !! nr_192

cipher256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
cipher256 key a = stateMatrixToOutput . mxor rf 
                . subBytes . shiftRows 
                . foldl cipherRound initState $ rs
  where
    initState = mxor (inputToStateMatrix a) r0
    roundKeys = expandKey nk_256 nb nr_256 (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr_256-1) . tail $ roundKeys
    rf = roundKeys !! nr_256

invcipher128 key a = stateMatrixToOutput . mxor rf . invSubBytes
                   . invShiftRows . foldl invCipherRound initState $ rs
  where
    roundKeys = reverse . expandKey nk_128 nb nr_128 $ (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr_128-1) . tail $ roundKeys
    rf = roundKeys !! nr_128
    initState = mxor (inputToStateMatrix a) r0

invcipher192 key a = stateMatrixToOutput . mxor rf . invSubBytes
                   . invShiftRows . foldl invCipherRound initState $ rs
  where
    roundKeys = reverse . expandKey nk_192 nb nr_192 $ (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr_192-1) . tail $ roundKeys
    rf = roundKeys !! nr_192
    initState = mxor (inputToStateMatrix a) r0

invcipher256 key a = stateMatrixToOutput . mxor rf . invSubBytes
                   . invShiftRows . foldl invCipherRound initState $ rs
  where
    roundKeys = reverse . expandKey nk_256 nb nr_256 $ (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr_256-1) . tail $ roundKeys
    rf = roundKeys !! nr_256
    initState = mxor (inputToStateMatrix a) r0

cipherRound instate roundkey = mxor roundkey . mixColumns
                           . shiftRows . subBytes $ instate

invCipherRound instate roundkey = invMixColumns . mxor roundkey
                                . invSubBytes . invShiftRows $ instate

tstate :: Mat.Matrix Word8
tstate = Mat.fromLists [[0x19,0xa0,0x9a,0xe9],
                        [0x3d,0xf4,0xc6,0xf8],
                        [0xe3,0xe2,0x8d,0x48],
                        [0xbe,0x2b,0x2a,0x08]]

tinput :: BS.ByteString
tinput = BS.pack [0x32,0x43,0xf6,0xa8
         ,0x88,0x5a,0x30,0x8d
         ,0x31,0x31,0x98,0xa2
         ,0xe0,0x37,0x07,0x34]


tkey = BS.pack [0x2b,0x7e,0x15,0x16
       ,0x28,0xae,0xd2,0xa6
       ,0xab,0xf7,0x15,0x88
       ,0x09,0xcf,0x4f,0x3c]

c1_plaintext = BS.pack [0x00,0x11,0x22,0x33
                       ,0x44,0x55,0x66,0x77
                       ,0x88,0x99,0xaa,0xbb
                       ,0xcc,0xdd,0xee,0xff]

c1_key = BS.pack [0x00,0x01,0x02,0x03
         ,0x04,0x05,0x06,0x07
         ,0x08,0x09,0x0a,0x0b
         ,0x0c,0x0d,0x0e,0x0f]

c1_output = BS.pack [0x69,0xc4,0xe0,0xd8
                    ,0x6a,0x7b,0x04,0x30
                    ,0xd8,0xcd,0xb7,0x80
                    ,0x70,0xb4,0xc5,0x5a]

c1_plaintext1 = toBytes $ hexString "00112233445566778899aabbccddeeff"
c1_key1 = toBytes $ hexString "000102030405060708090a0b0c0d0e0f"

c2_plaintext = toBytes $ hexString "00112233445566778899aabbccddeeff"
c2_key = toBytes $ hexString "000102030405060708090a0b0c0d0e0f1011121314151617"

c3_plaintext = toBytes $ hexString "00112233445566778899aabbccddeeff"
c3_key = toBytes $ hexString "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

