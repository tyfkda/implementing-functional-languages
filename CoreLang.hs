module CoreLang where

-- 1.3 Data types for the Core language

data Expr a
  = EVar Name         -- Variables
  | ENum Int          -- Numbers
  | EConstr Int Int   -- Constructor tag arity
  | EAp (Expr a) (Expr a)  -- Applications
  | ELet                   -- Let(rec) expressions
      IsRec                --   boolean with True = recursive,
      [(a, Expr a)]        --   Definitions
      (Expr a)             --   Body of let(rec)
  | ECase                  -- Case expression
      (Expr a)             --   Expression to scrutinise
      [Alter a]            --   Alternatives
  | ELam [a] (Expr a)      -- Lambda abstractions
  deriving (Show)

type CoreExpr = Expr Name

type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

coreProgram :: CoreProgram
coreProgram = [
  ("main", [], (EAp (EVar "double") (ENum 21))),
  ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))
  ]

-- 1.4 A small standard prelude

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x"),
      ("K", ["x", "y"], EVar "x"),
      ("K1", ["x", "y"], EVar "y"),
      ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                 (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f", "g", "x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

-- 1.5 A pretty-printer for the Core language
--- 1.5.1 Pretty-printing using strings

pprExpr :: CoreExpr -> Iseq
--pprExpr (ENum n) = show n
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline,
              iStr "  ", iIndent (pprDefns defns), iNewline,
              iStr "in ", pprExpr expr ]
    where keyword | not isrec = "let"
                  | isrec     = "letrec"

pprAExpr :: CoreExpr -> String
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = "(" ++ pprExpr e ++ ")"

--- 1.5.2 An abstract data type for pretty-printing

data Iseq = INil

iNil :: Iseq  -- The empty iseq
iStr :: String -> Iseq  -- Turn a string into an iseq
iAppend :: Iseq -> Iseq -> Iseq  -- Append two iseqs
iNewline :: Iseq  -- New line with indentation
iIndent :: Iseq -> Iseq  -- Indent an iseq
iDisplay :: Iseq -> String  -- Turn an iseq into a string

iConcat :: [Iseq] -> Iseq
iConcat [] = iNil
iConcat [seq] = seq
iConcat (seq: seqs) = seq `iAppend` iConcat seqs

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave c [seq] = seq
iInterleave c (seq: seqs) = iConcat [seq, c, iInterleave c seqs]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprint prog = iDisplay (pprProgram prog)

-- Main

main = do
  print coreProgram
