module Parser where

import Data.Char (isAlpha, isDigit)
import Data.List (foldl')
import CoreLang (CoreProgram, CoreScDefn, CoreExpr, Expr(..))

type Token = String  -- A token is never empty

syntax = take_first_parse . pProgram
  where take_first_parse ((prog,[]) : others) = prog
        take_first_parse (parse : others)     = take_first_parse others
        take_first_parse other                = error "Syntax erorr"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where mk_sc name paramNames eq coreExpr = (name, paramNames, coreExpr)

pExpr :: Parser CoreExpr
pExpr = ((pApply (pOneOrMore pAexpr) mk_ap_chain) `pAlt`
         (pThen4 (\_ defns _ expr -> ELet False defns expr) (pLit "let") pDefns (pLit "in") pExpr) `pAlt`
         (pThen4 (\_ defns _ expr -> ELet True defns expr) (pLit "letrec") pDefns (pLit "in") pExpr) `pAlt`
         (pThen4 (\_ expr _ alts -> ECase expr alts) (pLit "case") pExpr (pLit "of") pAlternatives))
  where mk_ap_chain [x] = x
        mk_ap_chain (x:xs) = foldl' EAp x xs

pAexpr :: Parser CoreExpr
pAexpr = ((pApply pVar EVar) `pAlt`
          (pApply pNum ENum) `pAlt`
          (pThen3 (\_ e _ -> e) (pLit "(") pExpr (pLit ")")))

--pDefns :: Parser [(String, Expr a)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

--pDefn :: Parser (String, Expr a)
pDefn = pThen3 (\var _ expr -> (var, expr)) pVar (pLit "=") pExpr

pAlternatives = pOneOrMoreWithSep pAlternative (pLit ";")

pAlternative = pThen6 (\_ num _ vars _ expr -> (num, vars, expr)) (pLit "<") pNum (pLit ">") (pZeroOrMore pVar) (pLit "->") pExpr

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

-- 1.6.2 Basic tools for parsing

type Parser a = [Token] -> [(a, [Token])]

pSat :: (String -> Bool) -> Parser String
pSat _ [] = []
pSat f (tok:toks) | f tok = [(tok, toks)]
                  | otherwise = []

pLit :: String -> Parser String
pLit s = pSat (== s)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar :: Parser String
pVar = pSat (\tok@(c:cs) -> isAlpha c && all isIdChar cs &&
                            not (tok `elem` keywords))

pNum :: Parser Int
pNum = pApply (pSat $ all isDigit) read

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pThen combine p1 p2 toks =
    [(combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                              (v2, toks2) <- p2 toks1]

pThen3 combine p1 p2 p3 toks =
    [(combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks,
                                 (v2, toks2) <- p2 toks1,
                                 (v3, toks3) <- p3 toks2]

pThen4 combine p1 p2 p3 p4 toks =
    [(combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks,
                                    (v2, toks2) <- p2 toks1,
                                    (v3, toks3) <- p3 toks2,
                                    (v4, toks4) <- p4 toks3]

pThen5 combine p1 p2 p3 p4 p5 toks =
    [(combine v1 v2 v3 v4 v5, toks5) | (v1, toks1) <- p1 toks,
                                       (v2, toks2) <- p2 toks1,
                                       (v3, toks3) <- p3 toks2,
                                       (v4, toks4) <- p4 toks3,
                                       (v5, toks5) <- p5 toks4]

pThen6 combine p1 p2 p3 p4 p5 p6 toks =
    [(combine v1 v2 v3 v4 v5 v6, toks6) | (v1, toks1) <- p1 toks,
                                          (v2, toks2) <- p2 toks1,
                                          (v3, toks3) <- p3 toks2,
                                          (v4, toks4) <- p4 toks3,
                                          (v5, toks5) <- p5 toks4,
                                          (v6, toks6) <- p6 toks5]

pZeroOrMore :: Parser a -> Parser [a]
--pZeroOrMore p = ((pOneOrMore p) `pAlt` (pEmpty []))
pZeroOrMore p = take 1 . ((pOneOrMore p) `pAlt` (pEmpty []))

pOneOrMore :: Parser a -> Parser [a]
--pOneOrMore p = pThen (:) p (pZeroOrMore p)
pOneOrMore p = take 1 . pThen (:) p (pZeroOrMore p)

pEmpty :: a -> Parser a
pEmpty v toks = [(v, toks)]

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f v, toks1) | (v, toks1) <- p toks]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 toks = if null r2
                                 then [([v1], toks1)]
                                 else [(v1': v2', toks3) | (v1', _) <- [r1],
                                                           (v2', toks3) <- pZeroOrMoreWithSep toks2]
  where (r1@(v1, toks1): _) = p1 toks
        r2 = p2 toks1
        toks2 = snd $ head r2
        pZeroOrMoreWithSep = ((pOneOrMoreWithSep p1 p2) `pAlt` (pEmpty []))
