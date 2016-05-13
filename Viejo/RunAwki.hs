module RunAwki(runAwki) where 
import AwkiSA
import Memoria
import Data.Map (Map)

runAwki :: AwkiProg -> String -> String
runAwki (AwkiProg prog) [] = "Entrada Vacia"
-- runAwki (AwkiProg [(x,y)]) entrada = if evaPat x then (recLineas y (lines entrada)) else ""
runAwki (AwkiProg prog) entrada = runAwkiSt (lines entrada) prog iniciar


runAwkiSt :: [String] -> [(Patron, Statement)] -> Map String Valor -> String
runAwkiSt [] _ _ = ""
-- runAwkiSt [s:sx] (x,y) map = 
 
 
evaPat :: Patron -> Bool
evaPat BEGIN = True
evaPat END = False
evaPat (Pat exp) = evaExpr exp /= 0 && evaExpr exp /= (Str "") 

evaStatement :: Statement -> String
evaStatement Empty = ""
-- evaStatement (Simple exp) = evaExpr exp
-- evaStatement (Print sal) = putStrLn (unlines (evaArrExp sal))
-- evaStatement Exit
-- evaStatement Sequence [Statement]
-- evaStatement (If exp stmnt1 stmnt2) = if evaExpr exp == 1 then evaStatement stmnt1 else evaStatement stmnt2
-- evaStatement For Expr Expr Expr Statement
-- evaStatement While Expr Statement = 
-- evaStatement DoWhile Statement Expr

evaArrExp :: [Expr] -> String
evaArrExp [] = "\n"
evaArrExp (x:xs) = show (evaExpr x) ++ "\t" ++ (evaArrExp xs)

evaExpr :: Expr -> Valor
evaExpr (Lit val) = val
-- evaExpr (Var str) = obtener str
-- evaExpr (Op2 bop exp1 exp2) = evaOp2 bop exp1 exp2
-- evaExpr (Op1 uop exp) = evaOp1 uop exp
-- evaExpr (Assign str exp) = insertar str (evaExpr exp)
-- evaExpr (Accum bop str exp) = acumular bop str exp
-- evaExpr (PP bool bool str) = evaPP bool bool str
-- evaExpr (Field exp) = evaField exp

evaOp2 :: BOp -> Expr -> Expr -> Valor
evaOp2 op p1 p2 =  ((evaBOp op) (evaExpr p1) (evaExpr p2))

evaOp1 :: UOp -> Expr -> Valor
evaOp1 op p = ((evaUOp op) (evaExpr p))

evaBOp :: BOp -> (Valor -> Valor -> Valor)
evaBOp  Add = (+)
evaBOp  Sub = (-)
evaBOp  Mul = (*)
evaBOp  Div = (\a b -> Num (quot (toInt a) (toInt b)))
evaBOp  Mod = (\a b -> Num (mod (toInt a) (toInt b)))
evaBOp  Lt = fBoolToVal (<)
evaBOp  Gt = fBoolToVal (>)
evaBOp  Le = fBoolToVal (<=)
evaBOp  Ne = fBoolToVal (/=)
evaBOp  Ge = fBoolToVal (>=)
evaBOp  Equal = fBoolToVal (==)
evaBOp  And = (\a b -> if ((&&) (toBool a)  (toBool b)) then 1 else 0)
evaBOp  Or = (\a b -> if ((||) (toBool a)  (toBool b)) then 1 else 0)
evaBOp  Concat = concatv

evaUOp :: UOp -> (Valor -> Valor)
evaUOp Plus = (\a -> a)
evaUOp Minus = (negate)
evaUOp Not = (\a -> if a == 0 then 1 else 0)

fBoolToVal :: (Valor -> Valor -> Bool) -> (Valor -> Valor -> Valor)
fBoolToVal fun = (\a b -> if fun a b then 1 else 0)