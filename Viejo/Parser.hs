{-# LANGUAGE StandaloneDeriving #-}
module Parser where

import AwkiSA
import ParseLib
import Data.Char(isDigit,isAlpha,isAlphaNum)

-- Se definen instancias automáticas de la clase Show
-- para todos los tipos vinculados a la sintaxis Awki
-- (se requiere el pragma StandaloneDeriving)
deriving instance Show UOp
deriving instance Show BOp
deriving instance Show Expr
deriving instance Show Statement
deriving instance Show Patron
deriving instance Show AwkiProg

{-- lexer básico, divide el string en tokens

Los tokens son todos strings:

    - operadores: ++ --  && ...
    - números:    123 -123
    - cadenas:    "hola"
    - identificadores: i var k3
    - puntuación:  (){},;
--}
lexer :: String -> [String]
lexer "" = []
-- espacios
lexer (sp:cs) | sp `elem` " \t\n" = lexer cs
-- operadores
lexer ('+':'+':cs) = "++" : lexer cs
lexer ('-':'-':cs) = "--" : lexer cs
lexer ('&':'&':cs) = "&&" : lexer cs
lexer ('|':'|':cs) = "||" : lexer cs
-- operadores de la forma +=
lexer (c:'=':cs)
  |c `elem` "+*/%-!<>=" =  (c:"=") : lexer cs
-- cadenas (no se guarda la última comilla: "hola )
lexer ('"':cs) = let (as,bs) = span (/='"') cs
                 in ('"':as) : if null bs then []
                                      else lexer $ tail bs
lexer (c:cs)
             -- números
             | isDigit c = let (ds,rs) = span isDigit cs
                           in  (c:ds) : lexer rs
             -- identificadores
             | isAlpha c =  let (ds,rs) = span isAlphaNum cs
                           in  (c:ds) : lexer rs
             -- puntuación (o caracteres extraños)
             | otherwise = [c] : lexer cs
                           

-- algunos combinadores útiles

-- secuencia que ignora el primero
(>*) :: Parse a b -> Parse a c -> Parse a b
p1 >* p2 = (p1 >*> p2) `build` fst
-- secuencia que ignora segundo
(*>) :: Parse a b -> Parse a c -> Parse a c
p1 *> p2 = (p1 >*> p2) `build` snd
-- entre paréntesis
paren :: Parse String a -> Parse String a
paren p = token "(" *> p >* token ")"
          
-- reconocimiento de expresiones según precedencia
-- atómos: numeros, cadenas, identificadores
--         operadores unarios
expr0, num, string, var, 
       parExpre, varpp, field, 
       unaryExpr0 :: Parse String Expr
expr0 = num `alt` 
        string `alt`
        var  `alt`
        field `alt`
        varpp `alt`
        unaryExpr0 `alt`
        parExpre 
num = spot (all isDigit)
      `build` (Lit  . Num . read)
string = spot isStr `build` (Lit . Str . tail)
  where isStr ('"':_) = True
        isStr _       = False
var = spot isIdent
      `build` Var
    where isIdent xs@(c:cs) =
            isAlpha c && all isAlphaNum cs &&
            xs `notElem` ["exit", "print", "for", "while", "if", "else","do","BEGIN","END"]
-- campos          
field = (token "$"  >*>  expr0) `build` \(_,e) -> Field e
-- incremento/decremento
varpp = ((token "++" >*> var) `build` \(_ ,Var v) -> PP True True v)
         `alt`
        ((token "--" >*> var) `build` \(_, Var v) -> PP True False v)
         `alt`
        ((var >*> token "++") `build` \(Var v,_) -> PP False True v)
        `alt`
        ((var >*> token "--") `build` \(Var v,_) -> PP False False v)        
-- operadores unarios
unaryExpr0 = (uop >*> expr0) `build` uncurry Op1
uop :: Parse String UOp
uop = ( token "!" `build` const Not)
      `alt`
      (token "-" `build` const Minus)
      `alt`
      (token "+" `build` const Plus)
-- expresión entre paréntesis      
parExpre =
  ( token "(" >*>
    expre     >*>
    token ")"
  ) `build` (fst.snd)


-- expre1 :  * / %  (asociativo a la izquierda)
expre1 :: Parse String Expr
expre1 = ( expr0 >*>
           list (opM >*> expr0) )
         `build` mkExpr

mkExpr :: (Expr , [(BOp,Expr)]) -> Expr
mkExpr (a,fs) = foldl operar a fs
operar :: Expr -> (BOp,Expr) -> Expr
operar e (op,a) = Op2 op e a

opM :: Parse String BOp
opM =  (token "*" `build` const Mul)
       `alt`
       (token "/" `build` const Div)
       `alt`
       (token "%" `build` const Mod)

-- expre2:  + -  (asociativo a la izquierda)
expre2 :: Parse String Expr
expre2 = ( expre1 >*>
           list (opA >*> expre1) )
         `build` mkExpr

opA :: Parse String BOp
opA =  (token "+" `build` const Add)
       `alt`
       (token "-" `build` const Sub)
       

-- expre3: concat (asociativo a la izquierda)
-- A diferencia de awk, se usa un operador
-- explícito de concatenación:  . (punto)
expr3 :: Parse String Expr
expr3 =   (expre2 >*>
           list (opConcat >*> expre2))
          `build` mkExpr
opConcat :: Parse String BOp
opConcat = token "." `build` const Concat

-- expre4: relacionales (no asociativo)
expre4 :: Parse String Expr
expre4 = expr3 `alt`
         ((expr3 >*> rel >*> expr3)
          `build` \(a,(r,b)) -> Op2 r a b)
rel :: Parse String BOp
rel = (token ">" `build` const Gt)
      `alt`
      (token "<" `build` const Lt)
      `alt`
      (token ">=" `build` const Ge)
      `alt`
      (token "<=" `build` const Le)
      `alt`
      (token "==" `build` const Equal)
      `alt`
      (token "!=" `build` const Ne)

-- expre5:  &&   (asociativo a la izquierda)
expre5 :: Parse String Expr
expre5 = (expre4 >*>
          list (opand >*> expre4))
         `build` mkExpr
opand :: Parse String BOp  
opand = token "&&" `build` const And         

-- expre6:  ||  (asociativo a la izquierda)
expre6 :: Parse String Expr
expre6 = (expre5 >*>
          list (opor >*> expre5))
         `build` mkExpr
opor :: Parse String BOp  
opor = token "||" `build` const Or         

-- expre7: = += *= -= (asociativo a la derecha)
expre7 :: Parse String Expr
expre7 = expre6 `alt` assign `alt` acc
assign, acc :: Parse String Expr
assign =  (var >*> opassign >*> expre7)
          `build` \(Var v,(_,e)) -> Assign v e
                                   
opassign :: Parse String ()
opassign = token "=" `build` const ()

acc = (var >*> opacc >*> expre7)
      `build` \(Var v,(op,e)) -> Accum op v e
opacc :: Parse String BOp
opacc = (token "+=" `build` const Add)
      `alt`
      (token "*=" `build` const Mul)
      `alt`
      (token "/=" `build` const Div)
      `alt`
      (token "-=" `build` const Sub)
      `alt`
      (token "%=" `build` const Mod)

-- expresiones en general
expre :: Parse String Expr
expre = expre7



-- instrucciones
statement :: Parse String Statement
statement =
  -- instrucción nula
  succeed Empty
  `alt`
  -- expresión
  (expre `build` Simple)
  `alt`
  -- exit
  (token "exit" `build` const Exit)
  `alt`
  -- print
  sprint `alt`
  -- instrucciones de control
  sif `alt` ssequence
  `alt` sfor `alt` swhile `alt` sdo

sprint,sif, ssequence,
       sfor, swhile, sdo:: Parse String Statement
sprint = (token "print" 
          *>
          printArguments )
         `build`  Print
printArguments :: Parse String [Expr]
printArguments = succeed [Field (Lit 0)]
                 `alt`
                 ((expre >*> list (token "," *> expre))
                  `build` uncurry (:))
sif = (token "if"
       *>
       paren  expre
       >*>
       statement
       >*>
       optional (
         token "else" *> statement)
       ) `build` \ (e,(s,lelse)) ->
                   If e s (if null lelse then Empty else head lelse)

swhile =
  (token "while"
   *>
   paren expre
   >*>
   statement) `build` uncurry While

sdo = (token "do" *>
       statement  >*>
       token "while" *>
       paren expre )
      `build` uncurry DoWhile

sfor =
  (token "for" *>
   paren ( expre >* token ";" >*>
           expre >* token ";" >*>
           expre )
   >*>
   statement)
  `build` \ ((e1,(e2,e3)),s) -> For e1 e2 e3 s

-- las instrucciones en una secuencia
-- están separadas siempre por ; (punto y coma)
-- Difiere con awk que admite "\n" como separador
ssequence =
  (token "{" *>
   statement >*>
   list (token ";" *> statement)
   >* token "}")
  `build` \(s,ss) -> Sequence (s : ss)

-- patrones
patron :: Parse String Patron
patron = (expre `build` Pat)
         `alt`
         (token "END" `build` const END)
         `alt`
         (token "BEGIN" `build` const BEGIN)


-- acciones
accion :: Parse String Statement
accion =  ssequence

-- reglas
regla :: Parse String (Patron,Statement)
regla =
  (patron `build` (\pat -> ( pat , print0)) ) 
  `alt`
  (accion  `build` \ac -> (pat0,ac) )
  `alt`
  (patron >*> accion)
  where print0 = Print [Lit 0]
        pat0  = Pat $ Lit 1

-- programa Awki
-- Las reglas deben separarse por ;
-- difiere de awk que admite también espacios 
prog :: Parse String AwkiProg
prog = (regla >*>
        list (token ";" *> regla)) `build`  (AwkiProg . uncurry (:))

-- ejecución del parser sobre un string
tryParse :: Parse String b -> String -> Maybe b
tryParse p xs
  = case filter (null.snd)  $ p (lexer xs) of
     [(e,[])] -> Just e
     _        -> Nothing

