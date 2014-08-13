{- Feed this program with the source file downloaded from
   http://nemesis.lonestar.org/computers/tandy/software/apps/m4/qd/opcodes.html

   or ../opcodes.html, this program will spit out with a formatted instruction
   list that can be read by the assembler as specification.

   Bash:
    $ cat opcodes.html | \
        runghc utils/genInstList.hs | \
        tee assembler/instructions.macro


   This tool is a part of `projz' project.

-}


import Control.Monad
import Data.List
import Data.Char
import Text.XML.Light

elementToStr :: Element -> String
elementToStr = join . map contentToStr . elContent

contentToStr :: Content -> String
contentToStr (CRef x) = x
contentToStr (Elem x) = elementToStr x
contentToStr (Text x) = cdData x


filterLine :: String -> Bool
filterLine x = p1 x && p2 x && p3 x
  where p1 = ("<td>" `isPrefixOf`)
        p2 = not . ("<td>---" `isPrefixOf`)
        p3 x = let len = length $ onlyElems $ parseXML x in len == 6

procLine :: String -> [String]
procLine = map elementToStr .
           select .
           onlyElems .
           parseXML
  where select xs = [xs !! 4, xs !! 0, xs !! 1]
                    -- op code
                    -- instruction
                    -- parameter


stripOpCode :: [String] -> [String]
stripOpCode (x:xs) = (takeWhile isHex x):xs
  where isHex c = isDigit c || c `elem` "ABCDEF"


main :: IO ()
main = do lns <- liftM (filter filterLine . lines) getContents
          putStr $ unlines $ map (unwords . stripOpCode . procLine) lns
