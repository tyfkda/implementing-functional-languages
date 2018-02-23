import Parser
import PrettyPrint (pprint)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = let mk_greeting hg name exclamation = (hg, name)
            in pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!")

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

main = do
  --let tokens = ["goodbye", "James", "!", "hello", "yoko", "!"]
  --print $ pGreeting tokens
  --print $ pGreetings tokens
  --print $ pGreetingsN tokens

  --print $ pOneOrMoreWithSep pGreeting (pLit ";") ["goodbye", "James", "!", ";", "hello", "yoko", "!"]

  putStrLn $ pprint $ syntax ["f", "=", "3"]
  putStrLn $ pprint $ syntax ["g", "x", "y", "=", "let", "z", "=", "x", "in", "z"]
  putStrLn $ pprint $ syntax ["h", "x", "=", "case", "(", "let", "y", "=", "x", "in", "y", ")", "of", "<", "1", ">", "->", "2", ";", "<", "2", ">", "->", "5"]
