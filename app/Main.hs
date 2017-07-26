module Main where

import Rjindael
import qualified Data.ByteString.Char8 as BSC
import Data.HexString

main :: IO ()
main = do
    let a = cipher128 tkey tinput
    putStrLn $ show $ fromBytes a
