{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Processor where

import Control.Lens hiding (ix)
import Control.Lens.At

import Data.Word (Word8, Word16)
import Data.Array (Array)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.Maybe

-- See: http://en.wikipedia.org/wiki/Intel_8080
-- for details of the specification

infixl 1 |>

(|>) :: a -> (a -> b) -> b
d |> f = f d

-- 8080 has 5 flag bits, in composite into a register
data Flag = Flag { _flg_s  :: Bool -- sign
                 , _flg_z  :: Bool -- zero
                 , _flg_p  :: Bool -- partial
                 , _flg_c  :: Bool -- carry
                 , _flg_ac :: Bool -- aux carry
                 }
makeLenses ''Flag

-- 8080 provides the following registers
data RegisterBundle = RegisterBundle { _reg_a    :: Word8 -- accumulator
                                     , _reg_flag :: Flag -- flags, see above
                                     , _reg_b    :: Word8
                                     , _reg_c    :: Word8
                                     , _reg_d    :: Word8
                                     , _reg_e    :: Word8
                                     , _reg_h    :: Word8
                                     , _reg_l    :: Word8
                                     , _reg_sp   :: Word16 -- stack pointer
                                     , _reg_pc   :: Word16 -- program counter
                                     }
makeLenses ''RegisterBundle

type Addr = Word16
type Memory64K = Array Addr Word8

data State = State { _registers :: RegisterBundle
                   , _memory    :: Memory64K
                   }
makeLenses ''State

{-
mMemLens :: forall f. (Applicative f) => Addr ->
            (Word8 -> f Word8) -> Memory64K -> f Memory64K
mMemLens = ix
-}


mMemLens :: Addr -> Traversal' Memory64K Word8
mMemLens = ix


-- S Z 0 A 0 P 1 C
flagIso :: Iso' Flag Word8
flagIso = iso to_ from_
  where to_   f = pack [_flg_s f, _flg_z f, False, _flg_ac f,
                       False, _flg_p f, True, _flg_c f]
        from_ w = Flag { _flg_s  = w `testBit` 7
                       , _flg_z  = w `testBit` 6
                       , _flg_ac = w `testBit` 4
                       , _flg_p  = w `testBit` 2
                       , _flg_c  = w `testBit` 1
                       }
        pack = foldl (\acc b -> acc * 2 + boolToInt b) 0
        boolToInt b = if b then 1 else 0


data Reg8 = A | B | C | D | E | F | H | L -- F: Flags
data Reg16 = PSW | BC | DE | HL | SP | PC

rRegLens :: Reg8 -> Lens' RegisterBundle Word8
rRegLens A = reg_a
rRegLens B = reg_b
rRegLens C = reg_c
rRegLens D = reg_d
rRegLens E = reg_e
rRegLens F = reg_flag . flagIso
rRegLens H = reg_h
rRegLens L = reg_l


rRegLens16Comp :: Lens' RegisterBundle Word8 ->
                  Lens' RegisterBundle Word8 ->
                  Lens' RegisterBundle Word16
rRegLens16Comp hb lb = lens get_ set_
  where get_ :: RegisterBundle -> Word16
        get_ r = let h = r ^. hb
                     l = r ^. lb
                 in ((fromIntegral h `shiftL` 8) .&. fromIntegral l)
        set_ :: RegisterBundle -> Word16 -> RegisterBundle
        set_ r w = r & hb .~ fromIntegral (w .&. 0xFF00 `shiftR` 8)
                     & lb .~ fromIntegral (w .&. 0x00FF)

rRegLens16 :: Reg16 -> Lens' RegisterBundle Word16
rRegLens16 HL = rRegLens16Comp reg_h reg_l
rRegLens16 BC = rRegLens16Comp reg_b reg_c
rRegLens16 DE = rRegLens16Comp reg_d reg_e
rRegLens16 PSW = rRegLens16Comp reg_a (reg_flag . flagIso)
rRegLens16 SP = reg_sp
rRegLens16 PC = reg_pc

regLens :: Reg8 -> Lens' State Word8
regLens x = registers . rRegLens x

regLens16 :: Reg16 -> Lens' State Word16
regLens16 x = registers . rRegLens16 x

memLens :: Addr -> Traversal' State Word8
memLens addr = memory . ix addr

regMem :: Reg16 -> Lens' State Word8
regMem x = lens get_ set_
  where get_ st   = fromJust $ st ^? memLens (st ^. regLens16 x)
        set_ st a = st &  memLens (st ^. regLens16 x) .~ a


regFlag :: Lens' State Flag
regFlag = registers . reg_flag

regFlagBin :: Lens' State Word8
regFlagBin = registers . reg_flag . flagIso


inst_mov :: Reg8 -> Reg8 -> State -> State
inst_mov dst src st = st & regLens dst .~ (st ^. regLens src)

inst_mov_tom :: Reg8 -> State -> State
inst_mov_tom src st = st & regMem HL .~ (st ^. regLens src)

inst_mov_fromm :: Reg8 -> State -> State
inst_mov_fromm dst st = st & regLens dst .~ (st ^. regMem HL)

inst_lxi :: Reg16 -> Word16 -> State -> State
inst_lxi dst w = regLens16 dst .~ w

inst_stax :: Reg16 -> State -> State
inst_stax dstm st = st & regMem dstm .~ (st ^. regLens A)

inst_inx :: Reg16 -> State -> State
inst_inx dst = regLens16 dst %~ (+1)

inst_inr :: Reg8 -> State -> State
inst_inr dst = regLens dst %~ (+1)

-- specialized case for `INR M` instruction
inst_inrm :: State -> State
inst_inrm = regMem HL %~ (+1)

inst_dcx :: Reg16 -> State -> State
inst_dcx dst = regLens16 dst %~ subtract 1

inst_dcr :: Reg8 -> State -> State
inst_dcr dst = regLens dst %~ subtract 1

-- specialized case for `DCR M` instruction
inst_dcrm :: State -> State
inst_dcrm = regMem HL %~ (subtract 1)

inst_mvi :: Reg8 -> Word8 -> State -> State
inst_mvi dst i = regLens dst .~ i

-- specialized case for `MVI M` instruction
inst_mvim :: Word8 -> State -> State
inst_mvim i = regMem HL .~ i

inst_rlc :: State -> State
inst_rlc st = st & regLens A     .~ new_a
                 & regFlag.flg_c .~ new_c
  where orig_c = if st ^. regFlag.flg_c then 1 else 0
        orig_a = st ^. regLens A
        new_a  = orig_a `shiftL` 1 .|. orig_c
        new_c  = orig_a `testBit` 7

inst_rrc :: State -> State
inst_rrc st = st & regLens A     .~ new_a
                 & regFlag.flg_c .~ new_c
  where orig_c = if st ^. regFlag.flg_c then 1 else 0
        orig_a = st ^. regLens A
        new_a  = orig_a `shiftR` 1 .|. orig_c `shiftL` 7
        new_c  = orig_a `testBit` 0

inst_dad :: Reg16 -> State -> State
inst_dad src st = st & regLens16 HL %~ (+foo)
  where foo = st ^. regLens16 src

-- Note: LDAX (XX) == MOV A,(XX)
inst_ldax :: Reg16 -> State -> State
inst_ldax srcm st = st & regLens A .~ (st ^. regMem srcm)


inst_nop :: State -> State
inst_nop = id


opcodeToInst :: Word8 -> Word8 -> Word8 -> State -> State
opcodeToInst = undefined


step1 :: State -> State
step1 st = let addr = st ^. registers . reg_pc
               (opcode, arg1, arg2) = fromJust $
                                      do opcode'<- st ^? memLens addr
                                         arg1'  <- st ^? memLens (addr + 1)
                                         arg2'  <- st ^? memLens (addr + 1)
                                         return (opcode',arg1',arg2')
               inst   = opcodeToInst opcode arg1 arg2
           in inst st


--inst_ldax :: RegisterName -> RegisterName -> State


main :: IO ()
main = return ()
