module Parser where

import Data.Char (isAlpha, isDigit)

type Token = String  -- A token is never empty

clex [] = []
clex (c:cs)
  | isWhiteSpace c = clex cs
  | isDigit c = let num_token = c : takeWhile isDigit cs
                    rest_cs = dropWhile isDigit cs
                in num_token : clex rest_cs
  | isAlpha c = let var_tok = c : takeWhile isIdChar cs
                    rest_cs = dropWhile isIdChar cs
                in var_tok : clex rest_cs
  | True = [c] : clex cs

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"
