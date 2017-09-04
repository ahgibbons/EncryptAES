module Hash where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.HexString

ch :: Bits a => a -> a -> a -> a
ch x y z = (x .&. y) `xor` (complement x .&. z)

parity :: Bits a => a -> a -> a -> a
parity x y z = x `xor` y `xor` z

maj :: Bits a => a -> a -> a -> a
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)


f :: Bits a => Int -> (a -> a -> a -> a)
f t 
  | 0 <= t && t < 20   = ch
  | 20 <= t && t < 30  = parity
  | 40 <= t && t < 60  = maj
  | 60 <= t && t <= 79 = parity
  | otherwise          = error "Invalid function index"

sum0_256 :: Bits a => a -> a
sum0_256 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22

sum1_256 :: Bits a => a -> a
sum1_256 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25

sigma0_256 :: Bits a => a -> a
sigma0_256 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3

sigma1_256 :: Bits a => a -> a
sigma1_256 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10

sum0_512 :: Bits a => a -> a
sum0_512 x = rotateR x 28 `xor` rotateR x 34 `xor` rotateR x 39

sum1_512 :: Bits a => a -> a
sum1_512 x = rotateR x 14 `xor` rotateR x 18 `xor` rotateR x 41

sigma0_512 :: Bits a => a -> a
sigma0_512 x = rotateR x 1 `xor` rotateR x 8 `xor` shiftR x 7

sigma1_512 :: Bits a => a -> a
sigma1_512 x = rotateR x 19 `xor` rotateR x 61 `xor` shiftR x 6

k_sha1 :: Int -> Word32
k_sha1 t | 0  <= t && t <= 19 = 0x5a827999
         | 20 <= t && t <= 39 = 0x6ed9eba1
         | 40 <= t && t <= 59 = 0x8f1bbcdc
         | 60 <= t && t <= 79 = 0xca62c1d6


--k_sha2_gen :: [String]
k_256 = concatMap (map (toWord . toBytes . hexString . BSC.pack) . words) ["428a2f98 71374491 b5c0fbcf e9b5dba5 3956c25b 59f111f1 923f82a4 ab1c5ed5",
              "d807aa98 12835b01 243185be 550c7dc3 72be5d74 80deb1fe 9bdc06a7 c19bf174",
              "e49b69c1 efbe4786 0fc19dc6 240ca1cc 2de92c6f 4a7484aa 5cb0a9dc 76f988da",
              "983e5152 a831c66d b00327c8 bf597fc7 c6e00bf3 d5a79147 06ca6351 14292967",
              "27b70a85 2e1b2138 4d2c6dfc 53380d13 650a7354 766a0abb 81c2c92e 92722c85",
              "a2bfe8a1 a81a664b c24b8b70 c76c51a3 d192e819 d6990624 f40e3585 106aa070",
              "19a4c116 1e376c08 2748774c 34b0bcb5 391c0cb3 4ed8aa4a 5b9cca4f 682e6ff3",
              "748f82ee 78a5636f 84c87814 8cc70208 90befffa a4506ceb bef9a3f7 c67178f2"]
  where
    toWord = BS.foldl' (\x y -> x * 256 + fromIntegral y) 0

k_512 = concatMap (map (toWord . toBytes . hexString . BSC.pack) . words) ["428a2f98d728ae22 7137449123ef65cd b5c0fbcfec4d3b2f e9b5dba58189dbbc",
       "3956c25bf348b538 59f111f1b605d019 923f82a4af194f9b ab1c5ed5da6d8118",
       "d807aa98a3030242 12835b0145706fbe 243185be4ee4b28c 550c7dc3d5ffb4e2",
       "72be5d74f27b896f 80deb1fe3b1696b1 9bdc06a725c71235 c19bf174cf692694",
       "e49b69c19ef14ad2 efbe4786384f25e3 0fc19dc68b8cd5b5 240ca1cc77ac9c65",
       "2de92c6f592b0275 4a7484aa6ea6e483 5cb0a9dcbd41fbd4 76f988da831153b5",
       "983e5152ee66dfab a831c66d2db43210 b00327c898fb213f bf597fc7beef0ee4",
       "c6e00bf33da88fc2 d5a79147930aa725 06ca6351e003826f 142929670a0e6e70",
       "27b70a8546d22ffc 2e1b21385c26c926 4d2c6dfc5ac42aed 53380d139d95b3df",
       "650a73548baf63de 766a0abb3c77b2a8 81c2c92e47edaee6 92722c851482353b",
       "a2bfe8a14cf10364 a81a664bbc423001 c24b8b70d0f89791 c76c51a30654be30",
       "d192e819d6ef5218 d69906245565a910 f40e35855771202a 106aa07032bbd1b8",
       "19a4c116b8d2d0c8 1e376c085141ab53 2748774cdf8eeb99 34b0bcb5e19b48a8",
       "391c0cb3c5c95a63 4ed8aa4ae3418acb 5b9cca4f7763e373 682e6ff3d6b2b8a3",
       "748f82ee5defb2fc 78a5636f43172f60 84c87814a1f0ab72 8cc702081a6439ec",
       "90befffa23631e28 a4506cebde82bde9 bef9a3f7b2c67915 c67178f2e372532b",
       "ca273eceea26619c d186b8c721c0c207 eada7dd6cde0eb1e f57d4f7fee6ed178",
       "06f067aa72176fba 0a637dc5a2c898a6 113f9804bef90dae 1b710b35131c471b",
       "28db77f523047d84 32caab7b40c72493 3c9ebe0a15c9bebc 431d67c49c100d4c",
       "4cc5d4becb3e42b6 597f299cfc657e2a 5fcb6fab3ad6faec 6c44198c4a475817"]
  where
    toWord = BS.foldl' (\x y -> x * 256 + fromIntegral y) 0

pad_sha1 :: BS.ByteString -> BS.ByteString
pad_sha1 m = 
  where
    len = BS.length m
    k   = (61 - 1 - len) `mod` 64
    m' = m' B.++ 
