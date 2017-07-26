module Main where

import Rjindael
import Tests
import qualified Data.ByteString.Char8 as BSC
import Data.HexString

main :: IO ()
main = do
    let a = encryptAES AES128 tkey tinput
    putStrLn $ show $ fromBytes a
