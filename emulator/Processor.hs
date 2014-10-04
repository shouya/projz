{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module Processor where

import Control.Lens
import Control.Monad.Reader
import Control.Category ((>>>))


import Data.Word (Word8, Word16)
import Data.Array
import Data.Bits

type Byte = Word8
type Word = Word16
type Addr = Word16

-- See: http://en.wikipedia.org/wiki/Intel_8080
-- for details of the specification

-- 8080 has 5 flag bits, in composite into a register
data Flag = Flag { _flg_s  :: Bool -- sign
                 , _flg_z  :: Bool -- zero
                 , _flg_p  :: Bool -- partial
                 , _flg_c  :: Bool -- carry
                 , _flg_ac :: Bool -- aux carry
                 }
makeLenses ''Flag

-- 8080 provides the following registers
data RegisterBundle = RegisterBundle { _reg_a    :: Byte -- accumulator
                                     , _reg_flag :: Flag -- flags, see above
                                     , _reg_b    :: Byte
                                     , _reg_c    :: Byte
                                     , _reg_d    :: Byte
                                     , _reg_e    :: Byte
                                     , _reg_h    :: Byte
                                     , _reg_l    :: Byte
                                     , _reg_sp   :: Word -- stack pointer
                                     , _reg_pc   :: Word -- program counter
                                     }

makeLenses ''RegisterBundle


data InputPortBundle =
  InputPortBundle { _in_r       :: Bool -- Reset
                  , _in_dma     :: Bool -- Direct memory access request
                  , _in_int     :: Bool -- Interrupt request
                  , _in_rdy     :: Bool -- Wait
                  , _in_d0      :: Bool
                  , _in_d1      :: Bool
                  , _in_d2      :: Bool
                  , _in_d3      :: Bool
                  , _in_d4      :: Bool
                  , _in_d5      :: Bool
                  , _in_d6      :: Bool
                  , _in_d7      :: Bool
                  , _in_d8      :: Bool
                  }
makeLenses ''InputPortBundle

data OutputPortBundle =
  OutputPortBundle { _out_d0    :: Bool
                   , _out_d1    :: Bool
                   , _out_d2    :: Bool
                   , _out_d3    :: Bool
                   , _out_d4    :: Bool
                   , _out_d5    :: Bool
                   , _out_d6    :: Bool
                   , _out_d7    :: Bool
                   , _out_a0    :: Bool
                   , _out_a1    :: Bool
                   , _out_a2    :: Bool
                   , _out_a3    :: Bool
                   , _out_a4    :: Bool
                   , _out_a5    :: Bool
                   , _out_a6    :: Bool
                   , _out_a7    :: Bool
                   , _out_a8    :: Bool
                   , _out_a9    :: Bool
                   , _out_aa    :: Bool
                   , _out_ab    :: Bool
                   , _out_ac    :: Bool
                   , _out_ad    :: Bool
                   , _out_ae    :: Bool
                   , _out_af    :: Bool
                   , _out_wait  :: Bool -- indicates that the processor
                                        -- is in the waiting state
                   , _out_rd    :: Bool -- Read
                   , _out_wr    :: Bool -- Write
                   , _out_s     :: Bool -- Active level indicates that the
                                        -- processor has put the "state
                                        -- word" on the data bus
                   , _out_ack_dma :: Bool
                   , _out_ack_int :: Bool
                   }
makeLenses ''OutputPortBundle


data State =
  State { _registers :: RegisterBundle
        , _input     :: InputPortBundle
        , _output    :: OutputPortBundle
        }
makeLenses ''State

type RegisterName = String


-- Read memory
refer :: RegisterName -> Lens' State Word
refer "M" = do h <- registers . reg_h
               l <- registers . reg_l
               return $ ((fromEnum h `shiftL` 8) .&. fromEnum l :: Word)


byteToBin :: Byte -> (Bool, Bool, Bool, Bool,
                      Bool, Bool, Bool, Bool)
byteToBin b = (b `testBit` 0,
               b `testBit` 1,
               b `testBit` 2,
               b `testBit` 3,
               b `testBit` 4,
               b `testBit` 5,
               b `testBit` 6,
               b `testBit` 7)

wordToBin :: Word -> (Bool, Bool, Bool, Bool,
                      Bool, Bool, Bool, Bool,
                      Bool, Bool, Bool, Bool,
                      Bool, Bool, Bool, Bool)
wordToBin w = (w `testBit` 0,
               w `testBit` 1,
               w `testBit` 2,
               w `testBit` 3,
               w `testBit` 4,
               w `testBit` 5,
               w `testBit` 6,
               w `testBit` 7,
               w `testBit` 8,
               w `testBit` 9,
               w `testBit` 10,
               w `testBit` 11,
               w `testBit` 12,
               w `testBit` 13,
               w `testBit` 14,
               w `testBit` 15)

requestRead :: Addr -> State -> State
requestRead addr =
  addr >>>
  \(d0,d1,d2,d3,d4,d5,d6,d7,
    d8,d9,da,db,dc,dd,de,df) ->
  over registers $ (do set out_d0 d0
                       set out_d1 d1)



inst_move :: RegisterName -> RegisterName -> State -> State
inst_move "A" "A" = over registers $ (view reg_a >>= set reg_a)


--inst_ldax :: RegisterName -> RegisterName -> State


main :: IO ()
main = return ()
