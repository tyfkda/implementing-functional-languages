import CoreLang (CoreProgram, Expr(..), recursive, nonRecursive)
import PrettyPrint (pprint)

-- Main

coreProgram :: CoreProgram
coreProgram = [
  ("main", [], (EAp (EVar "double") (ENum 21))),
  ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))),
  ("quadruple", ["x"], (ELet nonRecursive
                             [("twice_x", (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))]
                             (EAp (EAp (EVar "+") (EVar "twice_x")) (EVar "twice_x")))),
  ("isOne", ["x"], (ECase (EVar "x") [(1, [], (EVar "True")), (2, [], (EVar "False"))])),
  ("foo", ["x", "y", "z", "w"], (EAp (EAp (EVar "-")
                                          (EAp (EAp (EVar "*")
                                                    (EVar "x"))
                                               (EVar "y")))
                                     (EAp (EAp (EVar "/")
                                               (EVar "z"))
                                          (EVar "w")))),
  ("bar", ["x", "y", "z", "w"], (EAp (EAp (EVar "*")
                                          (EAp (EAp (EVar "+")
                                                    (EVar "x"))
                                               (EVar "y")))
                                     (EAp (EAp (EVar "-")
                                               (EVar "z"))
                                          (EVar "w")))),
  ("baz", ["x", "y", "p", "xs"], (EAp (EAp (EVar ">")
                                           (EAp (EAp (EVar "+")
                                                     (EVar "x"))
                                                (EVar "y")))
                                      (EAp (EAp (EVar "*")
                                                (EVar "p"))
                                           (EAp (EVar "length")
                                                (EVar "xs")))))
  ]

main = do
  putStrLn $ pprint coreProgram
