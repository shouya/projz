
module Memory (Memory64K) where

-- 8080 supports a 64 RAM
type AddressType = Word
type Memory64K = Array AddressType Byte
