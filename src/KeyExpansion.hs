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


