{-# LANGUAGE OverloadedStrings #-}
module Rjindael where

import qualified Data.ByteString as BS
import qualified Data.Bits as B
import GaloisFields
import Data.Bits ((.&.),(.|.),xor)

import Data.Char (intToDigit)
import Data.Word
import Data.Bits.ByteString
import Data.List.Split (chunksOf)
import qualified Data.Matrix as Mat


import Utils
import KeyExpansion (expandKey)

data AES = AES128 | AES192 | AES256 deriving (Show,Eq)
type PlainText = BS.ByteString
type Key = BS.ByteString
type CipherText = BS.ByteString

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


encryptAES :: AES -> Key -> PlainText -> CipherText
encryptAES aestype key text = stateMatrixToOutput . mxor rf 
                . subBytes . shiftRows 
                . foldl cipherRound initState $ rs
  where
    (nk,nr) = case aestype of
                   AES128 -> (nk_128, nr_128)
                   AES192 -> (nk_192, nr_192)
                   AES256 -> (nk_256, nr_256)
    initState = mxor (inputToStateMatrix text) r0
    roundKeys = expandKey nk nb nr (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr-1) . tail $ roundKeys
    rf = roundKeys !! nr
              
decryptAES :: AES -> Key -> CipherText -> PlainText
decryptAES aestype key text = stateMatrixToOutput . mxor rf . invSubBytes
                   . invShiftRows . foldl invCipherRound initState $ rs
  where
    (nk,nr) = case aestype of
                AES128 -> (nk_128, nr_128)
                AES192 -> (nk_192, nr_192)
                AES256 -> (nk_256, nr_256)
    roundKeys = reverse . expandKey nk nb nr $ (BS.unpack key)
    r0 = head roundKeys
    rs = take (nr-1) . tail $ roundKeys
    rf = roundKeys !! nr
    initState = mxor (inputToStateMatrix text) r0


cipherRound instate roundkey = mxor roundkey . mixColumns
                           . shiftRows . subBytes $ instate

invCipherRound instate roundkey = invMixColumns . mxor roundkey
                                . invSubBytes . invShiftRows $ instate


