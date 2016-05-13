module Memoria (iniciar, actualizarLinea, insertar, acumular, obtener) where  
import Data.Map (Map)
import AwkiSA
import qualified Data.Map as M
import Control.Monad.State

iniciar :: Map String Valor
iniciar  = M.insert "$0" (Str "") (M.insert "NR" 0 (M.insert "NF" 0 M.empty))

actualizarLinea :: String -> State (Map String Valor) ()
actualizarLinea s = state $ \mem -> ((), do--state $ \mem -> ((), (M.insert "$0" (Str s) (M.insert "NR" ((+) (fst $ runState $ obtener "NR" mem) 1) (M.insert "NF" (Num (length (words s))) (runState actualizarPesos (length (words s)) (words s) mem)))))\
		insertar "$0" (Str s)
		a <- obtener "NR"
		insertar "NR" (a)
		insertar "NF" (Num (length (words s)))
		)
		-- insertar 


actualizarPesos :: Int -> [String] -> State (Map String Valor) ()
actualizarPesos 1 s = state $ \mem -> ((), (M.insert "$1" (Str (head s))) mem)
actualizarPesos n s = state $ \mem -> ((), do
	insertar ("$" ++ show n) (Str (last s))
	actualizarPesos (n-1) (init s)
	)

--M.insert (("$" ++ (show n)) (Str (last s)) (runState (actualizarPesos (n-1) (init s)) mem))))

insertar :: String -> Valor -> State (Map String Valor)()
insertar key val  = state $ \mem -> ((), M.insert key val mem)

acumular :: (Valor -> Valor -> Valor) -> String -> Valor -> State (Map String Valor) ()
acumular fun key val = state $ \mem -> ((), M.insertWith' fun key val mem)

obtener :: String -> State (Map String Valor) Valor
obtener key = state $ \mem -> (M.findWithDefault (Str "NE") key mem, mem)


