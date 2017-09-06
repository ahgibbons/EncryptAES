module Main where

--import System.IO
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Data.HexString

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error

import Tests (tkey,tiv)

import Utils (pkcs7, unpkcs7)
{-}
import Rjindael
import Types
import Block (cbcEncrypt, cbcDecrypt)
-}

main :: IO ()
main = do
    args <- getArgs
    let flag    = args !! 0
        fpath   = args !! 1
        outpath = args !! 2
        CryptoPassed cipher = cipherInit tkey :: CryptoFailable AES128
        Just iv             = makeIV tiv :: (Maybe (IV AES128))
    intext <- BS.readFile fpath
    let outtext = case flag of 
                   "-e" ->  cbcEncrypt cipher iv (pkcs7 16 intext)
                   "-d" ->  fromJust. unpkcs7 . cbcDecrypt cipher iv $ intext
    BS.writeFile outpath outtext
    putStrLn "Finished."

