import CoreLang (CoreExpr, Expr(..), pprExpr)

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
  where e2s = e2 : e2s

foldll :: (a -> b -> a) -> a -> [b] -> a
foldll = foldl  -- in Gofer standard prelude.

main = do
  mapM_ pp [1..30]
  where pp n = print $ pprExpr (mkMultiAp n (EVar "f") (EVar "x"))
