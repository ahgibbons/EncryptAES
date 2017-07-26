{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--module GaloisFields (GaloisField2 (..),xtimes,rconList,aes_GF2_mod,GF2 (..),gf2Inv) where
module GaloisFields where

import Data.Bits
import Data.Word
import Data.Matrix

infixl 7 .*.
infixl 6 .+.


aes_GF2_mod = 283 :: Int
          
data GF2 a = GF2 a deriving (Show,Eq)
data GF2_8 a = GF2_8 a a a a deriving (Show,Eq)



instance Num (GF2 Int) where
    (GF2 a) + (GF2 b) = GF2 (a `xor` b)
    (GF2 a) * (GF2 b) = GF2 (pmod (foldr addGF2 0 . multGF2' 1 a $ b) aes_GF2_mod)
    negate (GF2 a)    = GF2 a
    signum _          = GF2 1
    abs a             = a
    fromInteger i     = GF2 (fromIntegral i)

instance Num (GF2 Word8) where
    (GF2 a) + (GF2 b) = GF2 (a `xor` b)
    (GF2 a) * (GF2 b) = GF2 (fromIntegral (pmod (foldr addGF2 0 . multGF2' 1 a' $ b') aes_GF2_mod))
                        where
                          a' = fromIntegral a
                          b' = fromIntegral b
    negate (GF2 a)    = GF2 a
    signum _          = GF2 1
    abs a             = a
    fromInteger i     = GF2 (fromIntegral i)

(.*.) :: GaloisField2 a => a -> a -> a
(.*.) a b = multGF2 283 a b

(.+.) :: GaloisField2 a => a -> a -> a
(.+.) a b = addGF2 a b


            
class GaloisField2 a where
  addGF2 :: a -> a -> a
  multGF2 :: Int -> a -> a -> a

instance GaloisField2 Int where
  addGF2 a b = a `xor` b
  multGF2 m a b = pmod (foldr addGF2 0 . multGF2' 1 a $ b) m

instance GaloisField2 Word8 where
  addGF2 a b = a `xor` b
  multGF2 m a b = fromIntegral $ multGF2 m (fromIntegral a :: Int) (fromIntegral b :: Int) 

addGF2_8 :: (GaloisField2 a) => GF2_8 a -> GF2_8 a -> GF2_8 a
addGF2_8 (GF2_8 a3 a2 a1 a0) (GF2_8 b3 b2 b1 b0) =
    GF2_8 (addGF2 a3 b3) (addGF2 a2 b2) (addGF2 a1 b1) (addGF2 a0 b0)

multGF2_8_list :: [Word8] -> [Word8] -> [Word8]
multGF2_8_list [a3,a2,a1,a0] [b3,b2,b1,b0] = [d3,d2,d1,d0]
  where
    (GF2_8 d3 d2 d1 d0) = multGF2_8 (GF2_8 a3 a2 a1 a0) (GF2_8 b3 b2 b1 b0)
          
multGF2_8 :: (GaloisField2 a) => GF2_8 a -> GF2_8 a -> GF2_8 a
multGF2_8 (GF2_8 a3 a2 a1 a0) (GF2_8 b3 b2 b1 b0) = GF2_8 d3 d2 d1 d0
  where
    d3 = a3 .*. b0 .+. a2 .*. b1 .+. a1 .*. b2 .+. a0 .*. b3
    d2 = a3 .*. b3 .+. b0 .*. a2 .+. b1 .*. a1 .+. b2 .*. a0
    d1 = a1 .*. b0 .+. b1 .*. a0 .+. a3 .*. b2 .+. a2 .*. b3
    d0 = a0 .*. b0 .+. a3 .*. b1 .+. a2 .*. b2 .+. a1 .*. b3

rotWordGF2_8 :: GF2_8 a -> GF2_8 a
rotWordGF2_8 (GF2_8 a b c d) = (GF2_8 b c d a)
                  
                  
gf2Inv :: (Eq a, Enum a, Num a, GaloisField2 a) => a -> a
gf2Inv 0 = 0
gf2Inv a = snd . head . filter (\(v,n) -> v==1) . map (\i -> (multGF2 283 a i, i)) $ [0..255]

xtimes :: (Num a, GaloisField2 a) => Int -> a -> a
xtimes m a = multGF2 m 2 a

multGF2' :: Int -> Int -> Int -> [Int]
multGF2' n a b 
  | n > a     = []
  | otherwise = (n .&. a) * b : multGF2' (shiftL n 1) a b

rconList :: (Num a, GaloisField2 a) => [a]
rconList = 141 : iterate (xtimes aes_GF2_mod) 1

pmod :: Int -> Int -> Int
pmod a m
  | leadingM < leadingA = a
  | otherwise = pmod (shiftL m (leadingM - leadingA) `xor` a) m
  where
    leadingM = countLeadingZeros m
    leadingA = countLeadingZeros a

log2Int :: Int -> Int
log2Int d  = log2Int' d 0
  where
    log2Int' 0 t = t
    log2Int' d t = log2Int' (d `div` 2) (t+1)
