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

data State =
  State { _registers :: RegisterBundle
        , _memory    :: Memory64K
        }
makeLenses ''State

type RegisterName = String


-- Read memory address from register
registerLens :: RegisterName -> Lens' RegisterBundle Byte
registerLens "A" = reg_a
registerLens "B" = reg_b
registerLens "C" = reg_c
registerLens "D" = reg_d
registerLens "E" = reg_e
registerLens "H" = reg_h
registerLens "L" = reg_l

register16LensAux :: Lens' RegisterBundle Byte ->
                     Lens' RegisterBundle Byte ->
                     Lens' RegisterBundle Word
register16LensAux hb lb = lens get set
  where get :: RegisterBundle -> Word
        get r = let h = view hb r
                    l = view lb r
                in ((fromEnum h `shiftL` 8) .&. fromEnum l)
        set :: RegisterBundle -> Word -> RegisterBundle
        set r w = r & hb .~ fromIntegral (w .&. 0xFFFF0000 `shiftR` 8)
                    & lb .~ fromIntegral (w .&. 0xFFFF)


register16Lens :: RegisterName -> Lens' RegisterBundle Word
register16Lens "H" = register16Lens reg_h reg_l
register16Lens "B" = register16Lens reg_b reg_c
register16Lens "D" = register16Lens reg_d reg_e


inst_mov_reg :: RegisterName -> RegisterName -> State -> State
inst_mov_reg dst src =
  over registers $ view (registerLens src) >>= set (registerLens dst)

inst_mov_to_mem :: RegisterName -> State -> State
inst_mov_to_mem src st =
  over memory (writeMemory (st ^. registers . register16Lens "H")
                           (st ^. registers . registerLens src)) st

inst_mov_from_mem :: RegisterName -> State -> State
inst_mov_from_mem dst st =
  st & registers . registerLens src .~
         (readMemory (st ^. registers . register16Lens "H")
                     (st ^. memory))

inst_lxi :: RegisterName -> Word -> State -> State
inst_lxi reg word = registers . register16Lens reg .~ word

inst_stax :: RegisterName -> Byte -> State -> State
inst_stax reg byte st =
  over memory (writeMemory (st ^. registers . register16Lens reg) byte) st

inst_inx :: RegisterName -> State -> State
inst_inx reg st =
  st & memory %~ modifyMemory (st ^. registers . register16Lens reg) (st ^. memory)

opcodeToInst :: Byte -> Byte -> Byte -> (State -> State)


step1 :: State -> State
step1 st = let addr = st ^. registers . reg_pc
               opcode = readMemory addr (_memory st)
               arg1 = readMemory (addr + 1) (_memory st)
               arg2 = readMemory (addr + 2) (_memory st)
               inst = opcodeToInst opcode arg1 arg2
           in inst st


--inst_ldax :: RegisterName -> RegisterName -> State


main :: IO ()
main = return ()
