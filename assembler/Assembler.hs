module Assembler () where

{-

A 8080 CPU assembler.

-}

import Data.Char (toUpper, toLower)
import Data.Word
import Data.Bits

import Control.Monad (liftM)
import Control.Applicative ((*>), (<*))

import Text.ParserCombinators.Parsec
import qualified Data.ByteString as BS


data Address = ALbl String
             | AHex Word16
             | ALblOffset String Int
             | AHexOffset Word16 Int

data ParseResult = Label { llabel :: String }
                 | Plain { ibyteCode :: BS.ByteString }
                 | Goto  { gbyteCode :: BS.ByteString
                         , goffset :: Offset }


comp8x2 :: Word8 -> Word8 -> Word16
comp8x2 a b = ((fromEnum a) `shiftL` 8) .|. (fromEnum b)

-- Case insensitive
ciChar c = char (toLower c) <|> char (toUpper c)
ciStr  s = try (mapM ciChar s)

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


-- Shortcuts for registers
A = string "A"
B = string "B"
C = string "C"
D = string "D"
E = string "E"
H = string "H"
L = string "L"
HL = string "[HL]"



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
