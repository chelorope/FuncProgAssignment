module Memoria (iniciar, insertar, acumular, obtener) where
import Data.Map (Map)
import AwkiSA
import qualified Data.Map as M
import Control.Monad.State

iniciar :: Map String Valor
iniciar = M.insert "$0" (Str "") (M.insert "NR" 0 (M.insert "NF" 0 M.empty))

insertar :: String -> Valor -> State (Map String Valor)()
insertar key val  = state $ \mem -> ((), M.insert key val mem)

acumular :: (Valor -> Valor -> Valor) -> String -> Valor -> State (Map String Valor) ()
acumular fun key val = state $ \mem -> ((), M.insertWith' fun key val mem)

obtener :: String -> State (Map String Valor) Valor
obtener key = state $ \mem -> (M.findWithDefault (Str "NE") key mem, mem)

mManip :: State (Map String Valor) Valor
mManip = do 
	insertar "o" (Num 4)
	insertar "g" (Num 3)
	insertar "NR" (Num 1234)
	obtener "NR" 
	obtener "NR"
	insertar "ps" (Num 2)
	obtener "$0"

juan = runState mManip iniciar