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
--- 1.5.1 Pretty-printingp using strings

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr $ show n
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iStr "  ", iIndent (pprDefns defns), iStr "\nin ", pprExpr expr ]
    where keyword | not isrec = "let"
                  | isrec     = "letrec"
pprExpr (ECase x alts)
  = iConcat [ iStr "case ", pprExpr x, iStr " of\n",
              iInterleave (iStr ";\n")
                          (map (\(key, vars, body) -> iConcat [iStr "  <", iStr (show key), iStr "> ",
                                                               iConcat (map iStr vars),
                                                               iStr "-> ", pprExpr body])
                               alts) ]
pprExpr (ELam args body)
  = iConcat [ iStr "\\", iInterleave (iStr " ") (map iStr args),
              iStr " = ", pprExpr body ]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = iConcat [ iStr "(", pprExpr e, iStr ")" ]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave iNewline (map pprProg prog)

pprProg :: CoreScDefn -> Iseq
pprProg (name, args, body)
  = iConcat [ iStr name, iStr " ", iInterleave (iStr " ") (map iStr args),
              iStr " = ", iIndent (pprExpr body) ]

--- 1.5.3 Implementing iseq

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline
  deriving (Show)

--- 1.5.2 An abstract data type for pretty-printing

iNil :: Iseq  -- The empty iseq
iNil = INil
iStr :: String -> Iseq  -- Turn a string into an iseq
iStr str = f $ span (/= '\n') str
  where f (s1, "") = IStr s1
        f (s1, s2) = IStr s1 `iAppend` (iNewline `iAppend` iStr (tail s2))
iAppend :: Iseq -> Iseq -> Iseq  -- Append two iseqs
iAppend INil seq2 = seq2
iAppend seq1 INil = seq1
iAppend seq1 seq2 = IAppend seq1 seq2
iNewline :: Iseq  -- New line with indentation
iNewline = INewline
iIndent :: Iseq -> Iseq  -- Indent an iseq
iIndent seq = IIndent seq
iDisplay :: Iseq -> String  -- Turn an iseq into a string
iDisplay seq = flatten 0 [(seq, 0)]

iConcat :: [Iseq] -> Iseq
iConcat [] = iNil
iConcat [seq] = seq
iConcat (seq: seqs) = seq `iAppend` iConcat seqs

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave c [seq] = seq
iInterleave c (seq: seqs) = iConcat [seq, c, iInterleave c seqs]

flatten :: Int               -- Current column; 0 for first column
           -> [(Iseq, Int)]  -- Work list
           -> String         -- Result
flatten _ [] = ""
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IStr s, _) : seqs) = s ++ (flatten (col + length s) seqs)
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs) = '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, _) : seqs) = flatten col ((seq, col) : seqs)

space indent = replicate indent ' '

pprint prog = iDisplay (pprProgram prog)

-- Main

coreProgram :: CoreProgram
coreProgram = [
  ("main", [], (EAp (EVar "double") (ENum 21))),
  ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))),
  ("quadruple", ["x"], (ELet nonRecursive
                             [("twice_x", (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))]
                             (EAp (EAp (EVar "+") (EVar "twice_x")) (EVar "twice_x")))),
  ("isOne", ["x"], (ECase (EVar "x") [(1, [], (EVar "True")), (2, [], (EVar "False"))]))
  ]

main = do
  putStrLn $ pprint coreProgram
