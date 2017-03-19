module Parser where

import Data.Char (isAlpha, isDigit)

type Token = String  -- A token is never empty

clex [] = []
clex (c:cs)
  | isWhiteSpace c = clex cs
  | isLineComment c cs = clex $ skipLineComment cs
  | isDigit c = let num_token = c : takeWhile isDigit cs
                    rest_cs = dropWhile isDigit cs
                in num_token : clex rest_cs
  | isAlpha c = let var_tok = c : takeWhile isIdChar cs
                    rest_cs = dropWhile isIdChar cs
                in var_tok : clex rest_cs
  | isTwoCharOps c cs = (c: head cs: []) : clex (tail cs)
  | True = [c] : clex cs

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"

isLineComment '-' ('-':_) = True
isLineComment _ _         = False

skipLineComment cs = drop1 $ dropWhile (/= '\n') cs
  where drop1 [] = []
        drop1 (x:xs) = xs

isTwoCharOps p [] = False
isTwoCharOps p (c:cs)  = (p: c: []) `elem` twoCharOps

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]
