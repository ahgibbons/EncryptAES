module Main where

import Rjindael
import Tests
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.HexString
import Types

main :: IO ()
main = do
    let a = encryptAES AES128 tkey tinput
    putStrLn $ show $ fromBytes a
