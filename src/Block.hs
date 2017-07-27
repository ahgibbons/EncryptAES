module Block where

import Rjindael
import Utils
import Data.List.Split (chunksOf)

import qualified Data.ByteString as BS


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
