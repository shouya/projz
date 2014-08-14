module Assembler where

{-

A 8080 CPU assembler.

-}

-- import Data.Char (toUpper, toLower)
import Data.List (intercalate)
-- import Data.Word
-- import Data.Bits

import Text.Printf

import Control.Monad (liftM)
-- import Control.Applicative ((*>), (<*))

import Text.ParserCombinators.Parsec
import qualified Data.ByteString as BS



{- Instruction Specification List Parsing

   instruction list can be generated with the tool below
   `utils/genInstList.hs` from a opcode html file.
-}

data Register = A | B | C | D | E | H | L | M | PSW | SP | IP

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
    intercalate " " [showcode c, n, intercalate "," $ map show p]
    where showcode = concatMap (printf "%02x") . BS.unpack



instLen :: Instruction -> Integer
instLen (Inst { _instCode = c
              , _instParams = ps }) = fromIntegral (BS.length c) +
                                      sum (map paramLen ps)

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
                   return $ Inst { _instCode = code
                                 , _instName = name
                                 , _instParams = parm }

parseInstCode :: Parser (BS.ByteString)
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
              <|> (many digit >>= return . Parm . read)




{- End of instruction list parsing procedures -}

{-
data Register = A | B | C | D | E | H | L | M | PSW | SP | IP

data Parameter = Reg Register
               | Addr
               | Byte
               | Word
               | Parm Int
-}
{-
comp8x2 :: Word8 -> Word8 -> Word16
comp8x2 a b = ((fromEnum a) `shiftL` 8) .|. (fromEnum b)



parseLine :: Parser [ParseResult]
parseLine = do
  skipMany space
  lbl <- option [] parseLabel
  skipMany space
  inst <- option [] parseInstruction
  skipMany space
  optional parseComment
  skipMany space
  return (lbl ++ inst)


{-
    comma pair:
        MOV {A, B}         ==> ("A", "B")
        MOV {B, [HL]}      ==> ("B", "[HL]")
        MVI {A, 23h}       ==> ("A", 0x23)

    A-E,H,L,HL,...  :   Registers, return string
    I8,I16          :   Immediate value, 8 bits or 16 bits, return Word8/16
    Addr            :   Address in any form, return Address object

-}

parseCommaPair :: Parser a -> Parser b -> Parser (a,b)
parseCommaPair a b = try $ do
  a' <- a
  skipMany space
  char ','
  skipMany space
  b' <- b
  return (a', b')

parseHex :: (Num a) => Parser a
parseHex = liftM read $ liftM (:[]) hexDigit

parseByte :: Parser Word8
parseByte = do
  a <- parseHex
  b <- parseHex
  return (a * 256 + b)

-- Parse 16 bits hex immediate val (2 bytes)
parseHex2 :: Parser Word16
parseHex2 = do l <- parseByte
               h <- parseByte
               oneOf "hH"
               return (comp8x2 l h)

-- Parse 8 bits hex immediate val (1 byte)
parseHex1 :: Parser Word8
parseHex1 = parseByte <* oneOf "hH"

parseInt :: Parser Int
parseInt = liftM read $ many digit


parseAHexAddr :: Parser Address
parseAHexAddr = do char '['
                   h <- parseHex2
                   char ']'
                   return (AHex h)

parseLblStr :: Parser String
parseLblStr = do f <- (letter <|> char '_')
                 r <- many (letter <|> char '_' <|> digit)
                 return (f : r)

parseALblAddr :: Parser Address
parseALblAddr = liftM ALbl parseLblStr



parseAHexOffset :: Parser Address
parseAHexOffset = do char '['
                     h <- parseHex2
                     many space
                     char '+'
                     many space
                     o <- parseInt
                     char ']'
                     return $ AHexOffset h o

parseALblOffset :: Parser Address
parseALblOffset = do char '['
                     l <- parseLblStr
                     many space
                     char '+'
                     many space
                     o <- parseInt
                     char ']'
                     return $ ALblOffset l o


parseAddr :: Parser Address
parseAddr =  try parseAHexAddr
         <|> try parseALblAddr
         <|> try parseAHexOffset
         <|> try parseALblOffset



{- instruction parsing begins -}

parseInst :: String -> Parser ParseResult
parseInst "MOV" =  (parseCommaPair B B >> Plain (BS.pack 0x40))
               <|> (parse



loadInst :: IO [String]
loadInst = readFile "instructions.macro" >>= return . filter foo . unlines
  where foo x = not ("#" `isPrefixOf` x)


parseFile = liftM join $ (many (parseLine >> optional newline)) <* eof
-}
