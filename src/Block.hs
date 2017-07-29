{-# LANGUAGE OverloadedStrings #-}
module Block where

import Rjindael
import Utils
import Data.List.Split (chunksOf)
import Data.Bits (xor)
import qualified Data.ByteString as BS

import Types

ecbEncrypt :: AES -> Key -> PlainText -> CipherText
ecbEncrypt aestype key ptext = BS.concat ctext'
  where
    ptext' = chunksOfBS blocksize $ pkcs7 blocksize ptext
    ctext' = map (encryptAES aestype key) ptext'
    

ecbDecrypt :: AES -> Key -> CipherText -> Maybe PlainText
ecbDecrypt aestype key ctext = unpkcs7 $ BS.concat ptext'
  where
    ctext' = chunksOfBS blocksize ctext
    ptext' = map (decryptAES aestype key) ctext'

cbcEncrypt :: AES -> IV -> Key -> PlainText -> CipherText
cbcEncrypt aestype iv key ptext = BS.concat ctext'
  where
    ptext' = chunksOfBS blocksize . pkcs7 blocksize $ ptext
    ctext' = scanl (\c0 c1 -> encryptAES aestype key (xor c0 c1)) iv ptext'

cbcDecrypt :: AES -> Key -> CipherText -> Maybe PlainText
cbcDecrypt aestype key ctext = unpkcs7 $ BS.concat ptext'
  where
    ctext' = chunksOfBS blocksize ctext
    ptext' = zipWith (\ca cb -> xor (decryptAES aestype key cb) ca) ctext' (tail ctext')
    
