module AwkiSA where

import Data.Maybe(fromMaybe)

type Nombre = String


data Valor = Num Int
           | Str String

data Expr
  = Lit Valor                   -- valor simple
  | Var String                  -- variable
  | Op2 BOp Expr Expr           -- operación binaria,   a + b
  | Op1 UOp Expr                -- operación unaria,    -a
  | Assign String Expr          -- asignación           x = e
  | Accum BOp String Expr       -- asignación ac.,      x += e
  | PP Bool       --   prefijo
       Bool       --   incremento
       String     --   variable
  | Field Expr

data Statement
  = Empty
  | Simple Expr
  | Print [Expr]
  | Exit
  | Sequence [Statement] 
  | If Expr Statement Statement
  | For Expr Expr Expr Statement
  | While Expr Statement
  | DoWhile Statement Expr

-- operadores binarios
data BOp = Add
         | Sub
         | Mul
         | Div
         | Mod
         | Lt
         | Gt
         | Le
         | Ne
         | Ge
         | Equal
         | And
         | Or
         | Concat
           
-- operadores unarios
data UOp =  Plus
         | Minus
         | Not


data Patron
  = BEGIN
  | END
  | Pat Expr


newtype AwkiProg = AwkiProg [(Patron,Statement)]

-- conversión a entero
-- fuerza a 0 las cadenas no numéricas
toInt :: Valor -> Int
toInt = fromMaybe 0 . toIntMb

-- conversión a entero
-- falla (Nothing) cuando la cadena no es numérica
toIntMb :: Valor -> Maybe Int
toIntMb (Num n) = Just n
toIntMb (Str xs) =
  case reads xs of
    [(n,"")] -> Just n
    _        -> Nothing

-- conversión a bool
-- observar que en general:  toBool v /= (toInt v /= 0) 
toBool :: Valor -> Bool
toBool (Num m) = m /= 0
toBool (Str xs) = not (null xs)

-- conversión a cadena
instance Show Valor where
  show (Str xs) = xs
  show (Num m)  = show  m

-- Esta instancia permite operar con valores
-- como si fueran números.  
instance Num Valor where
   a + b = Num (toInt a + toInt b)
   a * b = Num (toInt a * toInt b)
   a - b = Num (toInt a - toInt b)
   abs = Num . abs . toInt
   signum = Num . signum . toInt
   fromInteger = Num . fromInteger

-- concatenación de valores
concatv :: Valor -> Valor -> Valor
concatv v1 v2 = Str $ show v1 ++ show v2

-- Igualdad entre valores
instance Eq Valor where
  a == b =
    case (toIntMb a, toIntMb b) of
      (Just a1 , Just b1) -> a1 == b1
      _                   -> show a == show b
  

-- comparaciones > >= < <=
instance Ord Valor where
  compare a b =
    case (toIntMb a, toIntMb b) of
      (Just a1 , Just b1) -> compare a1 b1
      _                   -> compare (show a) (show b)
  



