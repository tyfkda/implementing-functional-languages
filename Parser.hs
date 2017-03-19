module Parser where

import Data.Char (isAlpha, isDigit)

type Token = (Int, String)  -- A token is never empty

clex :: Int -> String -> [Token]
clex _ [] = []
clex lineNo (c:cs)
  | isNewLine c = clex (lineNo + 1) cs
  | isWhiteSpace c = clex lineNo cs
  | isLineComment c cs = clex (lineNo + 1) $ skipLineComment cs
  | isDigit c = let num_token = (lineNo, c : takeWhile isDigit cs)
                    rest_cs = dropWhile isDigit cs
                in num_token : clex lineNo rest_cs
  | isAlpha c = let var_tok = (lineNo, c : takeWhile isIdChar cs)
                    rest_cs = dropWhile isIdChar cs
                in var_tok : clex lineNo rest_cs
  | isTwoCharOps c cs = (lineNo, c: head cs: []) : clex lineNo (tail cs)
  | True = (lineNo, [c]) : clex lineNo cs

isIdChar, isWhiteSpace, isNewLine :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t"
isNewLine c = c == '\n'

isLineComment '-' ('-':_) = True
isLineComment _ _         = False

skipLineComment cs = drop1 $ dropWhile (/= '\n') cs
  where drop1 [] = []
        drop1 (x:xs) = xs

isTwoCharOps p [] = False
isTwoCharOps p (c:cs)  = (p: c: []) `elem` twoCharOps

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]
