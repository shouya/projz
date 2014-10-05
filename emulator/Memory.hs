module Memory (Memory64K) where

import Data.Word (Word16, Word8)

import Data.Array.Lens

-- 8080 supports a 64 RAM
type AddressType = Word16
type Memory64K = Array AddressType Word8

readMemory :: AddressType -> Memory64K -> Word8
readMemory addr = ix addr

writeMemory :: AddressType -> Word8 -> Memory64K -> Memory64K
writeMemory addr byte = ix addr .~ byte

modifyMemory :: AddressType -> (Word8 -> Word8) -> Memory64K -> Memory64K
modifyMemory addr f = ix addr %~ f
