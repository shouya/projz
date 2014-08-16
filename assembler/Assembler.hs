module Assembler where

{-

A 8080 CPU assembler.

-}

-- import Data.Char (toUpper, toLower)
import Data.List (intercalate)
import Data.Word
import Data.Bits
import qualified Data.Map as M


import Text.Printf

import Control.Monad (liftM)
-- import Control.Applicative ((*>), (<*))

import Text.ParserCombinators.Parsec
import qualified Data.ByteString as BS


{- Instruction Specification List Parsing

   instruction list can be generated with the tool below
   `utils/genInstList.hs` from a opcode html file.
-}

type LocalAddr = Word16

data Register = A | B | C | D | E | H | L | M | PSW | SP | IP
              deriving (Eq)

instance Show Register where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"
  show E = "E"
  show H = "H"
  show L = "L"
  show M = "[HL]"
  show PSW = "PSW"
  show SP = "SP"
  show IP = "IP"

data Parameter = Reg Register
               | Addr
               | Byte
               | Word
               | Parm Int

instance Show Parameter where
  show (Reg r)  = show r
  show Addr     = "[addr]"
  show Byte     = "xx"
  show Word     = "xxxx"
  show (Parm i) = show i

data Instruction = Inst { _instName :: String
                        , _instCode :: BS.ByteString
                        , _instParams :: [Parameter]
                        }

instance Show Instruction where
  show Inst { _instName = n
            , _instCode = c
            , _instParams = p } =
    unwords [showcode c, n, intercalate "," $ map show p]
    where showcode = concatMap (printf "%02x") . BS.unpack



instLen :: Instruction -> Integer
instLen (Inst { _instCode = c
              , _instParams = ps }) = fromIntegral (BS.length c) +
                                      sum (map paramLen ps)

paramsLen :: [Parameter] -> Integer
paramsLen = sum . map paramLen

paramLen :: Parameter -> Integer
paramLen Reg {}  = 0
paramLen Addr {} = 2
paramLen Byte {} = 1
paramLen Word {} = 2
paramLen Parm {} = 0


parseInstList :: String -> [Instruction]
parseInstList = concatMap parseline . lines
  where parseline str = case parse parseInstLine "" str of
          Left _  -> []  -- error 'parse failed'
          Right x -> [x]

parseInstLine :: Parser Instruction
parseInstLine = do code <- parseInstCode
                   _ <- space
                   name <- parseInstName
                   _ <- optional space
                   parm <- parseInstParams
                   return Inst { _instCode = code
                               , _instName = name
                               , _instParams = parm }

parseInstCode :: Parser BS.ByteString
parseInstCode = liftM (BS.singleton . read . ("0x"++)) (count 2 hexDigit)

parseInstName :: Parser String
parseInstName = many1 upper

parseInstParams :: Parser [Parameter]
parseInstParams = parseInstParam `sepBy` char ','

parseInstParam :: Parser Parameter
parseInstParam =  (char 'A' >> return (Reg A))
              <|> (char 'B' >> return (Reg B))
              <|> (char 'C' >> return (Reg C))
              <|> (char 'D' >> return (Reg D))
              <|> (char 'E' >> return (Reg E))
              <|> (char 'H' >> return (Reg H))
              <|> (char 'L' >> return (Reg L))
              <|> (char 'M' >> return (Reg M))  -- M == [HL]
              <|> (string "PSW" >> return (Reg PSW))
              <|> (string "SP" >> return (Reg SP))
              <|> (string "IP" >> return (Reg IP))
              <|> (string "word" >> return Word)
              <|> (string "address" >> return Addr)
              <|> (string "byte" >> return Byte)
              <|> liftM (Parm . read) (many digit)

{- End of instruction list parsing procedures -}

data AddrType = HexAddr LocalAddr
--              | HexOffset Word16 Integer
              | LblOffset String Integer

data Argument = RegA Register
              | AddrA AddrType
              | ByteA Word8
              | WordA Word16
              | ParmA Int

data Operation = Label String
               | Action Instruction [Argument]

type LabelTable = M.Map String LocalAddr



computeLabelTable :: [Operation] -> LabelTable
computeLabelTable = snd . foldl accm (0, M.empty)
  where accm (ptr, m) (Label l) = (ptr, M.insert l ptr m)
        accm (ptr, m) (Action i _) = (ptr + fromIntegral (instLen i), m)


expandOffset :: LabelTable -> AddrType -> LocalAddr
expandOffset _ (HexAddr addr) = addr
expandOffset tbl (LblOffset lbl off) = case M.lookup lbl tbl of
  Nothing -> error "Error: label not found"
  Just x  -> x + fromIntegral off


validateArguments :: [Parameter] -> [Argument] -> Bool
validateArguments ps as = length ps == length as &&
                          and (zipWith validateArgument ps as)

validateArgument :: Parameter -> Argument -> Bool
validateArgument (Reg r)   (RegA r')  = r == r'
validateArgument (Addr)    (AddrA _)  = True
validateArgument (Byte)    (ByteA _)  = True
validateArgument (Word)    (WordA _)  = True
validateArgument (Parm i)  (ParmA i') = i == i'
validateArgument _ _ = False

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8]

encodeAddress :: LocalAddr -> [Word8]
encodeAddress = encodeWord16

extractArgumentByteCode :: Argument -> BS.ByteString
extractArgumentByteCode (AddrA (HexAddr a)) = BS.pack $ encodeAddress a
extractArgumentByteCode (AddrA _) =
  error "Please expand the offsets of addresses before assembling."
extractArgumentByteCode (ByteA b) = BS.singleton b
extractArgumentByteCode (WordA w) = BS.pack $ encodeWord16 w
extractArgumentByteCode _         = BS.empty


assembleArguments :: [Parameter] -> [Argument] -> BS.ByteString
assembleArguments ps as =
  if validateArguments ps as then
    foldl BS.append BS.empty $ map extractArgumentByteCode as
  else error "Invalid Arguments"


assembleInstruction :: Instruction -> [Argument] -> BS.ByteString
assembleInstruction (Inst _ code params) args
  | paramsLen params == 0 = code
  | otherwise             = code `BS.append`
                            assembleArguments params args



assemble :: [Operation] -> BS.ByteString
assemble ops = foldl BS.append BS.empty $
               map (curryOp assembleInstruction) ops_addrexpanded
  where tbl = computeLabelTable ops
        ops_addrexpanded = map mapargs $ filter notlabel ops
        notlabel (Label _) = False
        notlabel _ = True
        mapargs (Action i args) = Action i $ map mapaddr args
        mapargs _ = undefined
        mapaddr (AddrA a) = AddrA $ HexAddr $ expandOffset tbl a
        mapaddr a         = a
        curryOp f (Action i a) = f i a
        curryOp _ _ = undefined
