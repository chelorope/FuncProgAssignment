-- Fabian Rydel - CI 4799631-5
-- Cyro Marcelo Rodriguez - CI 4256763-0

module Memoria (iniciar, actualizarLinea, insertar, acumular, obtener, pertenece, borrarPesos) where  
import Data.Map (Map)
import AwkiSA
import qualified Data.Map as M

iniciar :: Map String Valor
iniciar = M.insert "exit" (Num 0) (M.insert "$0" (Str "") (M.insert "NR" 0 (M.insert "NF" 0 M.empty)))

actualizarLinea :: String -> Map String Valor ->  Map String Valor
actualizarLinea s map = M.insert "$0" (Str s) (M.insert "NR" ((+) (obtener "NR" map) 1) (M.insert "NF" (Num (length (words s))) (actualizarPesos (length (words s)) (words s) (borrarPesos 1 map))))

borrarPesos :: Int -> Map String Valor -> Map String Valor
borrarPesos 0 map = map
borrarPesos n map = if M.member ("$" ++ (show n)) map then M.delete ("$" ++ (show n)) (borrarPesos (n+1) map) else map

actualizarPesos :: Int -> [String] -> Map String Valor -> Map String Valor
actualizarPesos 0 s map = map
actualizarPesos 1 s map = M.insert "$1" (Str (head s)) map
actualizarPesos n s map = M.insert ("$" ++ (show n)) (Str (last s)) (actualizarPesos (n-1) (init s) map)

insertar :: String -> Valor -> Map String Valor -> Map String Valor
insertar key val map  = M.insert key val map

acumular :: (Valor -> Valor -> Valor) -> String -> Valor -> Map String Valor -> Map String Valor
acumular fun key val map = M.insertWith' (flip fun) key val map

obtener :: String -> Map String Valor -> Valor
obtener key map = if M.member key map then
	map M.! key else
	Str ""

pertenece :: String -> Map String Valor -> Bool
pertenece key map = M.member key map