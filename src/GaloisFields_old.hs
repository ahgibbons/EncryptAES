{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GaloisFields (GaloisField2 (..),xtimes,rconList,aes_mod,GF2 (..)) where

import Data.Bits
import Data.Word
import Data.Matrix


aes_mod = 283 :: Int

data GF2 a = GF2 a deriving (Show,Eq)


instance Num (GF2 Int) where
    (GF2 a) + (GF2 b) = GF2 (a `xor` b)
    (GF2 a) * (GF2 b) = GF2 (pmod (foldr addGF2 0 . multGF2' 1 a $ b) aes_mod)
    negate (GF2 a)    = GF2 a
    signum _          = GF2 1
    abs a             = a
    fromInteger i     = GF2 (fromIntegral i)

instance Num (GF2 Word8) where
    (GF2 a) + (GF2 b) = GF2 (a `xor` b)
    (GF2 a) * (GF2 b) = GF2 (fromIntegral (pmod (foldr addGF2 0 . multGF2' 1 a' $ b') aes_mod))
                        where
                          a' = fromIntegral a
                          b' = fromIntegral b
    negate (GF2 a)    = GF2 a
    signum _          = GF2 1
    abs a             = a
    fromInteger i     = GF2 (fromIntegral i)


class GaloisField2 a where
  addGF2 :: a -> a -> a
  multGF2 :: Int -> a -> a -> a

instance GaloisField2 Int where
  addGF2 a b = a `xor` b
  multGF2 m a b = pmod (foldr addGF2 0 . multGF2' 1 a $ b) m

instance GaloisField2 Word8 where
  addGF2 a b = a `xor` b
  multGF2 m a b = fromIntegral $ multGF2 m (fromIntegral a :: Int) (fromIntegral b :: Int) 



xtimes :: (Num a, GaloisField2 a) => Int -> a -> a
xtimes m a = multGF2 m 2 a

multGF2' :: Int -> Int -> Int -> [Int]
multGF2' n a b 
  | n > a     = []
  | otherwise = (n .&. a) * b : multGF2' (shiftL n 1) a b

rconList :: (Num a, GaloisField2 a) => [a]
rconList = 141 : iterate (xtimes aes_mod) 1

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
