module Main(main) where

import RunAwki
import System.Environment
import Parser
import AwkiSA

main :: IO ()
main = do 
	args <- getArgs
	let src = unirArgs args
	putStrLn (show src)
	case tryParse prog src of
		Just prg ->  interact $ runAwki prg
		Nothing -> putStrLn "error: sintaxis incorrecta"

unirArgs :: [String] -> String
unirArgs [] = ""
unirArgs (x:xs) = show x ++ " " ++ unirArgs xs					  

progEjemplo :: AwkiProg
progEjemplo
  = AwkiProg [(Pat (Op2 Gt (Var "NF") (Lit 2)),
               Sequence [Print [Op2 Add (Field (Lit 1)) (Field (Var "NF"))]])]

entradaEjemplo :: String
entradaEjemplo =
  unlines ["1 2 56"
          ,"10 33"
          ,"8 30 0"
          ,"3"
          ,"8 1 2 3 4 5 6 790"
          ]
