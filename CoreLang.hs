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

pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2

pprAExpr :: CoreExpr -> String
pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e | otherwise      = "(" ++ pprExpr e ++ ")"


-- Main

main = do
  print coreProgram
