-- Fabian Rydel - CI 4799631-5
-- Cyro Marcelo Rodriguez - CI 4256763-0

module RunAwki(runAwki) where 
import AwkiSA
import Memoria
import Data.Map (Map)

------------------  RUN AWKI ----------------------------------------------------------------------------------------------------------

runAwki :: AwkiProg -> String -> String
runAwki (AwkiProg prog) entrada = show (fst respB) ++ show (fst respSt) ++ show (fst respE)
	where 
	respB = runAwkiBegin prog iniciar
	respSt = runAwkiSt prog (lines entrada) (snd respB)
	respE = runAwkiEnd prog (insertar "exit" (Num 0) (snd respSt))
	
--------------------

runAwkiBegin :: [(Patron, Statement)] -> Map String Valor -> (Valor, Map String Valor)
runAwkiBegin [] map = (Str "", map)
runAwkiBegin ((BEGIN,st):prx) map = (concatv resPrint resRecB, mapRecB)
	where 
	(resPrint, resB, newMapB) = evaStatement st map
	(resRecB, mapRecB) = runAwkiBegin prx newMapB
runAwkiBegin (p:prx) map = runAwkiBegin prx map

--------------------

runAwkiEnd :: [(Patron, Statement)] -> Map String Valor -> (Valor, Map String Valor)
runAwkiEnd [] map = (Str "", map)
runAwkiEnd ((END,st):prx) map = (concatv resPrint resRecE, mapRecE)
	where 
	(resPrint, resE, newMapE) = evaStatement st map
	(resRecE, mapRecE) = runAwkiEnd prx newMapE
runAwkiEnd (p:prx) map = runAwkiEnd prx map

--------------------

runAwkiSt :: [(Patron, Statement)] -> [String] -> Map String Valor -> (Valor, Map String Valor)
runAwkiSt _ [] map = (Str "", map)
runAwkiSt prog (s:sx) map =  if (toBool (obtener "exit" map) == False) && (toBool (obtener "error" map) == False) then (concatv res resRec, mapRec) else (Str "", map)
	where
	(res, newMap) = recorrerProg prog s (actualizarLinea s map)
	(resRec, mapRec) = runAwkiSt  prog sx newMap

---------------
recorrerProg ::[(Patron,Statement)] -> String -> Map String Valor -> (Valor, Map String Valor)
recorrerProg ((BEGIN,st):prx) s map = recorrerProg prx s map
recorrerProg ((END,st):prx) s map = recorrerProg prx s map
recorrerProg [] _ map = (Str "", map)
recorrerProg (p:px) s map = if (toBool (obtener "exit" map) == False) && (toBool (obtener "error" map) == False) then (if fst resp then  
	(concatv resPrint resRec , mapRec)
	else recorrerProg px s (snd resp)) else (Str "", map)
	where
	resp = evaPat (fst p) s map
	(resPrint, res, newMap) = evaStatement (snd p) (snd resp)
	(resRec, mapRec) = recorrerProg px s newMap
 
------------------  RUN AWKI ----------------------------------------------------------------------------------------------------------

------------------  PATRONES ----------------------------------------------------------------------------------------------------------

evaPat :: Patron -> String -> Map String Valor -> (Bool, Map String Valor )
evaPat BEGIN s map = (True, map)
evaPat END s map = (True, map)
evaPat (Pat exp) s map = (fst resp1 /= 0 && fst resp2 /= (Str ""), (snd resp2))
	where 
	resp1 = evaExpr exp map
	resp2 = (evaExpr exp (snd resp1))

------------------  PATRONES ----------------------------------------------------------------------------------------------------------

------------------  STATEMENTS ----------------------------------------------------------------------------------------------------------

evaStatement :: Statement -> Map String Valor -> (Valor, Valor, Map String Valor)
evaStatement Empty map = (Str "", Str "", map)
evaStatement (Simple exp) map = (Str "", fst (evaExpr exp map), snd (evaExpr exp map))
evaStatement (Print sal) map = (fst (evaPrint sal map), fst (evaPrint sal map), snd (evaPrint sal map))
evaStatement Exit map = (Str "", Str "", insertar "exit" (Num 1) map)
evaStatement (Sequence lst) map = evaSequence lst map
evaStatement (If exp stmnt1 stmnt2) map = if toBool (fst mapi) == True then evaStatement stmnt1(snd mapi) else evaStatement stmnt2 map
	where mapi = (evaExpr exp map)
evaStatement (For exp1 exp2 exp3 stmnt) map = evaFor exp2 exp3 stmnt (snd (evaExpr exp1 map))
evaStatement (While exp stmnt) map = evaWhile exp stmnt map
evaStatement (DoWhile stmnt expr) map = evaDoWhile expr stmnt map
---------------
evaPrint :: [Expr] -> Map String Valor  -> (Valor, Map String Valor)
evaPrint (x:[]) map = ((concatv (fst (evaExpr x map)) (Str "\n")), snd (evaExpr x map))
evaPrint (x:xs) map = (concatv res (concatv (Str "\t") resRec), mapRec)
	where 
	(res, newMap) = (evaExpr x map)
	(resRec, mapRec) = (evaPrint xs newMap)
---------------
evaSequence :: [Statement] -> Map String Valor -> (Valor, Valor, Map String Valor)
evaSequence [] map = (Str "", Str "", map)
evaSequence (st:rst) map = if (toBool (obtener "exit" map) == False) && (toBool (obtener "error" map) == False) then (concatv resPrint resRecPrint, concatv res resRec, mapRec) else (Str "", Str "", map)
	where 
	(resPrint, res, newMap) = evaStatement st map
	(resRecPrint, resRec, mapRec) = evaSequence rst newMap
---------------
evaFor :: Expr -> Expr -> Statement -> Map String Valor -> (Valor, Valor, Map String Valor)
evaFor exp2 exp3 stmnt map
	| bo == False = (Str "", Str "", map)
	| bo == True = ((concatv resPrint resRecPrint), (concatv res resRec), mapRec)
	where 
	bo = toBool (fst (evaExpr exp2 map))
	(resPrint, res, newMap) = (evaStatement stmnt map)
	(resRecPrint, resRec, mapRec) = evaFor exp2 exp3 stmnt (snd (evaExpr exp3 newMap))	
---------------
evaWhile :: Expr -> Statement -> Map String Valor -> (Valor, Valor, Map String Valor)
evaWhile ex stat map
	| bo == False = (Str "", Str "", map)
	| bo == True = ((concatv resPrint resRecPrint), (concatv res resRec) , mapRec)
	where 
	bo = toBool (fst (evaExpr ex map))
	(resPrint, res, newMap) = evaStatement stat map
	(resRecPrint, resRec, mapRec) = evaWhile ex stat newMap
---------------
evaDoWhile :: Expr -> Statement -> Map String Valor -> (Valor, Valor, Map String Valor)
evaDoWhile ex stat map = ((concatv resPrint (case bo of
												False -> Str ""
												True -> resRecPrint)),(concatv res (case bo of
																						False -> Str ""
																						True -> resRec)) , case bo of 
																													False -> newMap
																													True -> mapRec)
	where 
	bo = toBool (fst (evaExpr ex map))
	(resPrint, res, newMap) = evaStatement stat map
	(resRecPrint, resRec, mapRec) = evaWhile ex stat newMap

------------------  STATEMENTS ----------------------------------------------------------------------------------------------------------

------------------  EXPRESIONES ----------------------------------------------------------------------------------------------------------

evaExpr :: Expr -> Map String Valor -> (Valor, Map String Valor)
evaExpr (Lit val) map = (val,map)
evaExpr (Var str) map = (obtener str map, map)
evaExpr (Op2 bop exp1 exp2) map = evaOp2 bop exp1 exp2 map
evaExpr (Op1 uop exp) map = evaOp1 uop exp map
evaExpr (Assign str exp) map = ((fst $ evaExpr exp map), (insertar str (fst $ evaExpr exp map) map))
evaExpr (Accum bop str exp) map = (obtener str res, res)
	where res = if pertenece str map then (acumular (evaBOp bop) str (fst $ evaExpr exp map) map) else (acumular (evaBOp bop) str (fst $ evaExpr exp (insertar str (Num 0) map)) (insertar str (Num 0) map))
evaExpr (PP bool bool2 str) map = evaPP bool bool2 str map
evaExpr (Field exp) map = if (fst (evaExpr exp map)) >= (Num 0) then (obtener ("$" ++ show (fst (evaExpr exp map))) map, map) else (Str "ERROR", insertar "error" 1 map)
---------------
evaOp2 :: BOp -> Expr -> Expr -> Map String Valor -> (Valor, Map String Valor)
evaOp2 Div p1 p2 map = if toInt (fst exp2) /= 0 then (((evaBOp Div) (fst exp1) (fst exp2)), (snd exp2)) else (Str "ERROR", insertar "error" 1 map)
	where 
		exp1 = evaExpr p1 map
		exp2 = evaExpr p2 (snd exp1)
evaOp2 Mod p1 p2 map = if toInt (fst exp2) /= 0 then (((evaBOp Mod) (fst exp1) (fst exp2)), (snd exp2)) else (Str "ERROR", insertar "error" 1 map)
	where
		exp1 = evaExpr p1 map
		exp2 = evaExpr p2 (snd exp1)
evaOp2 op p1 p2 map = (((evaBOp op) (fst exp1) (fst exp2)), (snd exp2))
	where 
		exp1 = evaExpr p1 map
		exp2 = evaExpr p2 (snd exp1)
---------------
evaOp1 :: UOp -> Expr -> Map String Valor -> (Valor, Map String Valor)
evaOp1 op p map = (((evaUOp op) (fst map1)), (snd map1))
	where
		map1 = evaExpr p map
---------------
evaPP :: Bool -> Bool -> String -> Map String Valor -> (Valor, Map String Valor)
evaPP True True key map = (obtener key (acumular (+) key 1 map), (acumular (+) key 1 map))
evaPP False True key map = (obtener key map, (acumular (+) key 1 map))
evaPP True False key map = (obtener key (acumular (-) key 1 map), (acumular (-) key 1 map))
evaPP False False key map = (obtener key map, (acumular (-) key 1 map))
	
------------------  EXPRESIONES ----------------------------------------------------------------------------------------------------------

------------------  OPERADORES ----------------------------------------------------------------------------------------------------------

evaBOp :: BOp -> (Valor -> Valor -> Valor)
evaBOp  Add = (+)
evaBOp  Sub = (-)
evaBOp  Mul = (*)
evaBOp  Div = (\a b -> Num (quot (toInt a) (toInt b)))
evaBOp  Mod = (\a b -> Num (rem (toInt a) (toInt b)))
evaBOp  Lt = fBoolToVal (<)
evaBOp  Gt = fBoolToVal (>)
evaBOp  Le = fBoolToVal (<=)
evaBOp  Ne = fBoolToVal (/=)
evaBOp  Ge = fBoolToVal (>=)
evaBOp  Equal = fBoolToVal (==)
evaBOp  And = (\a b -> if ((&&) (toBool a)  (toBool b)) then 1 else 0)
evaBOp  Or = (\a b -> if ((||) (toBool a)  (toBool b)) then 1 else 0)
evaBOp  Concat = concatv
---------------
evaUOp :: UOp -> (Valor -> Valor)
evaUOp Plus = (\a -> a)
evaUOp Minus = (negate)
evaUOp Not = (\a -> if a == 0 then 1 else 0)
---------------
fBoolToVal :: (Valor -> Valor -> Bool) -> (Valor -> Valor -> Valor)
fBoolToVal fun = (\a b -> if fun a b then 1 else 0)

------------------  OPERADORES ----------------------------------------------------------------------------------------------------------