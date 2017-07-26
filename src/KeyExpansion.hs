{-# LANGUAGE OverloadedStrings #-}
module KeyExpansion where

import qualified Data.ByteString as BS
import qualified Data.Bits as B
import Data.Word
import Data.Bits (xor)
import qualified Data.Matrix as Mat
import Data.List.Split (chunksOf)

import Utils

nb_c = 4 :: Int
nk_128 = 4 :: Int
nk_192 = 6 :: Int
nk_256 = 8 :: Int
nr_128 = 10 :: Int
nr_192 = 12 :: Int
nr_256 = 14 :: Int

expandKey :: Int -> Int -> Int -> [Word8] -> [Mat.Matrix Word8]
expandKey nK nB nR key = map (Mat.transpose . Mat.fromLists) $ chunksOf 4 expanded
  where ws = expandKey' nK key
        expanded = ws ++ expandKey'' nK nK nR nB ws

expandKey' :: Int -> [Word8] -> [[Word8]]
expandKey' nK key = map (\n -> take 4 . drop (4*n) $ key) [0..nK-1]

expandKey'' :: Int -> Int -> Int -> Int -> [[Word8]] -> [[Word8]]
expandKey'' i nk nr nb ws 
  | i < nb*(nr+1) = let temp' = ws !! (nk-1)
                        imod = i `mod` nk
                        temp = if imod==0
                               then zipWith xor (subWord . rotWord $ temp') [rcon (i `div` nk),0,0,0]
                               else if (imod==4 && nk>6)
                                    then subWord temp'
                                    else temp'
                        wn   = zipWith xor (head ws) temp
                    in wn : expandKey'' (i+1) nk nr nb (tail ws ++ [wn])
  | otherwise     = []

tkey_128 = [0x2b,0x7e,0x15,0x16
       ,0x28,0xae,0xd2,0xa6
       ,0xab,0xf7,0x15,0x88
       ,0x09,0xcf,0x4f,0x3c] :: [Word8]

tkey_192 = [0x8e,0x73,0xb0,0xf7
           ,0xda,0x0e,0x64,0x52
           ,0xc8,0x10,0xf3,0x2b
           ,0x80,0x90,0x79,0xe5
           ,0x62,0xf8,0xea,0xd2
           ,0x52,0x2c,0x6b,0x7b] :: [Word8]

tkey_256 = [0x60,0x3d,0xeb,0x10
           ,0x15,0xca,0x71,0xbe
           ,0x2b,0x73,0xae,0xf0
           ,0x85,0x7d,0x77,0x81
           ,0x1f,0x35,0x2c,0x07
           ,0x3b,0x61,0x08,0xd7
           ,0x2d,0x98,0x10,0xa3
           ,0x09,0x14,0xdf,0xf4] :: [Word8]
