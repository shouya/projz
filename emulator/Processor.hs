{-# LANGUAGE TemplateHaskell #-}

module Processor where

import Control.Lens
import Control.Monad.Reader

import Data.Word
import Data.Array


type Byte = Word8
type Word = Word16


-- See: http://en.wikipedia.org/wiki/Intel_8080
-- for details of the specification

-- 8080 has 5 flag bits, in composite into a register
data Flag = Flag { _flg_s  :: Boolean -- sign
                 , _flg_z  :: Boolean -- zero
                 , _flg_p  :: Boolean -- partial
                 , _flg_c  :: Boolean -- carry
                 , _flg_ac :: Boolean -- aux carry
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
  InputPortBundle { _in_r       :: Boolean -- Reset
                  , _in_dma     :: Boolean -- Direct memory access request
                  , _in_int     :: Boolean -- Interrupt request
                  , _in_rdy     :: Boolean -- Wait
                  , _in_d0      :: Boolean
                  , _in_d1      :: Boolean
                  , _in_d2      :: Boolean
                  , _in_d3      :: Boolean
                  , _in_d4      :: Boolean
                  , _in_d5      :: Boolean
                  , _in_d6      :: Boolean
                  , _in_d7      :: Boolean
                  , _in_d8      :: Boolean
                  }
makeLenses ''InputPortBundle

data OutputPortBundle =
  OutputPortBundle { _out_d0    :: Boolean
                   , _out_d1    :: Boolean
                   , _out_d2    :: Boolean
                   , _out_d3    :: Boolean
                   , _out_d4    :: Boolean
                   , _out_d5    :: Boolean
                   , _out_d6    :: Boolean
                   , _out_d7    :: Boolean
                   , _out_a0    :: Boolean
                   , _out_a1    :: Boolean
                   , _out_a2    :: Boolean
                   , _out_a3    :: Boolean
                   , _out_a4    :: Boolean
                   , _out_a5    :: Boolean
                   , _out_a6    :: Boolean
                   , _out_a7    :: Boolean
                   , _out_a8    :: Boolean
                   , _out_a9    :: Boolean
                   , _out_aa    :: Boolean
                   , _out_ab    :: Boolean
                   , _out_ac    :: Boolean
                   , _out_ad    :: Boolean
                   , _out_ae    :: Boolean
                   , _out_af    :: Boolean
                   , _out_wait  :: Boolean -- indicates that the processor
                                           -- is in the waiting state
                   , _out_rd    :: Boolean -- Read
                   , _out_wr    :: Boolean -- Write
                   , _out_s     :: Boolean -- Active level indicates that the
                                           -- processor has put the "state
                                           -- word" on the data bus
                   , _out_ack_dma :: Boolean
                   , _out_ack_int :: Boolean
                   }
makeLenses ''OutputPortBundle


data State =
  State { _registers :: RegisterBundle
        , _input     :: InputPortBundle
        , _output    :: OutputPortBundle
        }
makeLenses ''Processor

_inst_move :: RegisterName -> RegisterName -> State -> State
_inst_move "A" "A" st = registers (reg_a

type Intruction :: World -> World
