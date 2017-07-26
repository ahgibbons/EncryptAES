{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Data.HexString
import qualified Data.ByteString as BS
import Rjindael 


tinput :: BS.ByteString
tinput = BS.pack [0x32,0x43,0xf6,0xa8
         ,0x88,0x5a,0x30,0x8d
         ,0x31,0x31,0x98,0xa2
         ,0xe0,0x37,0x07,0x34]

tkey = BS.pack [0x2b,0x7e,0x15,0x16
       ,0x28,0xae,0xd2,0xa6
       ,0xab,0xf7,0x15,0x88
       ,0x09,0xcf,0x4f,0x3c]


c1_plaintext = toBytes $ hexString "00112233445566778899aabbccddeeff"
c1_key = toBytes $ hexString "000102030405060708090a0b0c0d0e0f"
c1_ciphertext = toBytes $ hexString "69c4e0d86a7b0430d8cdb78070b4c55a"

c2_plaintext = toBytes $ hexString "00112233445566778899aabbccddeeff"
c2_key = toBytes $ hexString "000102030405060708090a0b0c0d0e0f1011121314151617"
c2_ciphertext = toBytes $ hexString "dda97ca4864cdfe06eaf70a0ec0d7191"

c3_plaintext = toBytes $ hexString "00112233445566778899aabbccddeeff"
c3_key = toBytes $ hexString "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
c3_ciphertext = toBytes $ hexString "8ea2b7ca516745bfeafc49904b496089"
