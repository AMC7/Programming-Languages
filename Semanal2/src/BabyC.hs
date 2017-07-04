{-
- Lenguajes de programacion
- 2017-1
- Favio E. Miranda Perea
- Susana H. Martin Lunas
- Fernando A. Galicia Mendoza

- Implementacion de BabyC.
-}

module BabyC where

import Data.List

-- | Nombre. Tipo que representa los nombres de variable.
type Nombre = String

-- | BabyC. Tipo que representa una expresion del lenguaje imperativo BabyC.
data BabyC = Var Nombre
         | VNum Int
         | VBool Bool
         | Suma BabyC BabyC
         | Resta BabyC BabyC
         | Prod BabyC BabyC
         | If BabyC BabyC BabyC
         | IZ BabyC
         | Let Nombre BabyC BabyC
         | Gt BabyC BabyC
         | Lt BabyC BabyC
         | Eq BabyC BabyC
         | Neq BabyC BabyC
         | Ge BabyC BabyC
         | Neg BabyC
         | Asig BabyC BabyC 
         | Ref BabyC 
         | Deref BabyC
         | L Int
         | Seq BabyC BabyC
         | While BabyC BabyC
         | Void deriving (Show,Eq)

-- | LDir. Alias para denotar una direccion de memoria.
type LDir = BabyC

-- | Val. Alias para denotar un valor.
type Val = BabyC

-- | Celda. Tipo que representa una celda de memoria, la cual es
-- un par cuya primera entrada es la direccion de memoria y la
-- segunda entrada el valor que guarda esa direccion.
type Celda = (LDir,Val)

-- | Mem. Tipo que representa una memoria.
type Mem = [Celda] 

-- | Sust. Definimos la sustitucion de variables como un par donde la primera
-- entrada es el nombre de la variable y la segunda entrada
-- la expresion que sustituira.
type Sust = (String,BabyC)

{-EJERCICIOS:-}
{-Semántica dinámica-}

-- | fv. Funcion que obtiene las variables libres de una expresion.
fv :: BabyC -> [Nombre]
fv f =  case f of Var x -> [x]
                  VNum x -> []
                  VBool x-> []
                  Suma x y -> union (fv x) (fv y)
                  Resta x y -> union (fv x) (fv y)
                  Prod x y -> union (fv x) (fv y)
                  If x y z -> union (fv x) (union (fv y) (fv z))
                  IZ x -> (fv x)
                  Let x y z -> [w | w <- fv (z), w /= x]
                  Gt x y ->union (fv x) (fv y)
                  Lt x y ->union (fv x) (fv y)
                  Eq x y ->union (fv x) (fv y)
                  Neq x y -> union (fv x) (fv y)
                  Ge x y -> union (fv x) (fv y)
                  Neg x-> fv x
                  Asig x y ->union (fv x) (fv y)
                  Ref x ->fv x
                  Deref x ->fv x
                  L x ->[]
                  Seq x y ->union (fv x) (fv y)
                  While x y ->union (fv x) (fv y)
                  Void -> []

-- | sustituye. Funcion que realiza una sustitucion.
sustituye :: BabyC -> Sust -> BabyC
sustituye bc (q,e) = case bc of  Var x ->  if x == q then e else bc
                                 VNum x -> bc
                                 VBool x-> bc
                                 Suma x y  ->Suma  (sustituye x (q,e))(sustituye y (q,e))
                                 Resta x y ->Resta (sustituye x (q,e))(sustituye y (q,e))
                                 Prod x y  ->Prod  (sustituye x (q,e))(sustituye y (q,e))
                                 If x y z  ->If    (sustituye x (q,e))(sustituye y (q,e)) (sustituye z (q, e) )
                                 IZ x      ->IZ    (sustituye x (q,e))
                                 Let x y z ->if elem x (union [q] (fv e)) then bc  else (Let x (sustituye y (q, e)) (sustituye z (q, e)))
                                 Gt x y    ->Gt    (sustituye x (q,e))(sustituye y (q,e))
                                 Lt x y    ->Lt    (sustituye x (q,e))(sustituye y (q,e))
                                 Eq x y    ->Eq    (sustituye x (q,e))(sustituye y (q,e))
                                 Neq x y   ->Neq   (sustituye x (q,e))(sustituye y (q,e))
                                 Ge x y    ->Ge    (sustituye x (q,e))(sustituye y (q,e))
                                 Neg x     ->Neg   (sustituye x (q,e)) 
                                 Asig x y  ->Asig  (sustituye x (q,e))(sustituye y (q,e))
                                 Ref x     ->Ref   (sustituye x (q,e))
                                 Deref x   ->Deref (sustituye x (q,e))
                                 L x       -> bc
                                 Seq x y   ->Seq   (sustituye x (q,e))(sustituye y (q,e))
                                 While x y ->While (sustituye x (q,e))(sustituye y (q,e))
                                 Void -> bc                    

-- | accessMem. Funcion que dado una direccion de memoria accede a ella.
accessMem :: LDir -> Mem -> Maybe BabyC
accessMem  direccion memoria = let celda = [(x,y)|(x,y)<-memoria , x == direccion] in if null celda then Nothing else Just (snd(head celda))

-- Te regresa el valor 
valDeMaybe ::Maybe BabyC -> BabyC
valDeMaybe Nothing = error "No se encuentra este valor"
valDeMaybe (Just a) = a

-- | update. Funcion que dada una celda y una memoria, actualiza
-- la misma celda en la memoria.
update :: Celda -> Mem -> Mem
update (direccion,valor) memoria =   if  accessMem direccion memoria == Nothing
                                             then error "No esta la celda que especificaste"
                                             else (direccion,valor):[(x,y)|(x,y)<- memoria, x/=direccion]    
                             
                                     
-- | newL. Funcion que dada una memoria genera una nueva direccion de memoria.
newL :: Mem -> LDir
newL memoria= let aux = dominio memoria in
                   if not(null(aux)) 
                        then (L (maximum aux +1))
                        else (L 0)

-- | dominio. Funcion que devuelve el dominio de una memoria.
dominio :: Mem -> [Int]
dominio memoria = [x|((L x),y)<- memoria ]


-- | eval1. Funcion que aplica reduccion a un paso.
eval1 :: (Mem,BabyC) -> (Mem,BabyC)
eval1 (memoria ,expresion )= case expresion of 
                                        (Var n) -> (memoria ,expresion )
                                        (VNum n)  -> (memoria ,expresion )
                                        (VBool u) -> (memoria ,expresion)
                                        (Suma (VNum x)(VNum y)) -> (memoria ,VNum (x+y))
                                        (Suma (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux , (Suma (VNum x) (snd aux)))
                                        (Suma t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Suma (snd aux) t2))
                                        (Resta (VNum x)(VNum y)) -> (memoria ,VNum (x-y))
                                        (Resta (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux ,Resta (VNum x) (snd aux))
                                        (Resta t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Resta (snd aux) t2))
                                        (Prod (VNum x)(VNum y)) -> (memoria ,VNum (x*y))
                                        (Prod (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux , (Prod (VNum x) (snd aux)))
                                        (Prod t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Prod (snd aux) t2))
                                        If (VBool x) y z  
                                                  | (x== True) ->(memoria,y)
                                                  | (x== False) ->(memoria,z) 
                                        If x y z  -> let aux = eval1 (memoria,x) in (fst aux , (If (snd aux) y z))
                                        IZ (VNum x)  
                                                  | x== 0 -> (memoria, VBool True)
                                                  | x/= 0 -> (memoria, VBool False)
                                        IZ x      ->let aux = eval1 (memoria,x) in (fst aux , (IZ (snd aux)))
                                        Let x y z -> case y of 
                                                          (Void) -> (memoria , sustituye z (x,y)) 
                                                          (Var y1) -> (memoria , sustituye z (x,y)) 
                                                          (VNum y1) -> (memoria , sustituye z (x,y)) 
                                                          (VBool y1) -> (memoria , sustituye z (x,y)) 
                                                          (L x1) -> (memoria , sustituye z (x,y))             
                                                          _-> let aux = eval1 (memoria ,y) in (fst aux ,Let x (snd aux)  z)
                                        (Gt (VNum x)(VNum y)) -> (memoria ,VBool (x>y))
                                        (Gt (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux , (Gt (VNum x) (snd aux)))
                                        (Gt t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Gt (snd aux) t2))
                                        (Lt (VNum x)(VNum y)) -> (memoria ,VBool (x<y))
                                        (Lt (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux , (Lt (VNum x) (snd aux)))
                                        (Lt t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Lt (snd aux) t2))
                                        (Eq (VNum x)(VNum y)) -> (memoria ,VBool (x==y))
                                        (Eq (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux , (Eq (VNum x) (snd aux)))
                                        (Eq t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Eq (snd aux) t2))
                                        (Neq (VNum x)(VNum y)) -> (memoria ,VBool (x/=y))
                                        (Neq (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux , (Neq (VNum x) (snd aux)))
                                        (Neq t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Neq (snd aux) t2))
                                        (Ge (VNum x)(VNum y)) -> (memoria ,VBool (x>=y))
                                        (Ge (VNum x) t2) -> let aux = eval1 (memoria,t2) in (fst aux , (Ge (VNum x) (snd aux)))
                                        (Ge t1 t2) -> let aux = eval1 (memoria,t1) in (fst aux , (Ge (snd aux) t2))
                                        Neg (VBool x) -> (memoria ,(VBool (not x)))
                                        Neg t1      ->let aux = eval1 (memoria,t1) in (fst aux , (Neg (snd aux)))   
                                        Asig (L x) y  -> case y of  
                                                          (Void) -> (update ((L x),y) memoria ,Void) 
                                                          (Var y1) -> (update ((L x),y) memoria ,Void) 
                                                          (VNum y1) -> (update ((L x),y) memoria ,Void) 
                                                          (VBool y1) -> (update ((L x),y) memoria ,Void) 
                                                          (L x1) -> (update ((L x),y) memoria ,Void)             
                                                          _-> let aux = eval1 (memoria,y) in (fst aux , (Asig (L x) (snd aux)))             
                                        Asig x y  ->let aux = eval1 (memoria,x) in (fst aux , (Asig (snd aux) y))
                                        Ref x     -> case x of  
                                                          (Void) -> let direccion = newL memoria in ((direccion,x):memoria,direccion)
                                                          (Var y1) -> let direccion = newL memoria in ((direccion,x):memoria,direccion)
                                                          (VNum y1) -> let direccion = newL memoria in ((direccion,x):memoria,direccion)
                                                          (VBool y1) -> let direccion = newL memoria in ((direccion,x):memoria,direccion)
                                                          (L x1) -> let direccion = newL memoria in ((direccion,x):memoria,direccion)      
                                                          _-> let aux = eval1 (memoria,x) in (fst aux , (Ref (snd aux)))    
                                        Deref x   ->let aux = (valDeMaybe (accessMem x (memoria))) in case x of  
                                                          (Void) -> (memoria, aux)
                                                          (Var y1) -> (memoria ,aux)
                                                          (VNum y1) -> (memoria ,aux)
                                                          (VBool y1) -> (memoria ,aux)
                                                          (L x1) -> (memoria , aux)
                                                          _-> let aux = eval1 (memoria,x) in (fst aux , (Deref (snd aux)))    
                                        L x       -> (memoria ,expresion)
                                        Seq x y   -> let aux = eval1 (memoria,x) in  case x of 
                                                          (Void) -> (fst aux ,y)
                                                          (Var y1) -> (fst aux ,y)
                                                          (VNum y1) -> (fst aux ,y)
                                                          (VBool y1) -> (fst aux , y)
                                                          (L x1) -> (fst aux ,y)
                                                          _-> let aux = eval1 (memoria,x) in (fst aux , (Seq (snd aux) y)) 
                                        While x y -> let evaluacion =evals (memoria,x) in
                                                        case evaluacion of (mem ,(VBool True)) -> (memoria , Seq (y) (While x y))
                                                                           (mem,(VBool False)) -> (memoria , Void)
                                                                           _->error "a"                                                               
                                        Void ->(memoria ,expresion )
                                   
                                        
prueba_Asignacion = eval1 ([((L 1),VNum 5)],(Asig (L 1)(VNum 20)))     
prueba_Asignacion_1 = eval1 ([((L 1),VNum 5) , ((L 2),VNum 45)],(Deref (L  2)))   
prueba_Asignacion_2 = eval1 ([],Seq (VNum 5) (Void) )                           
                     
                     
                                      
-- | evals. Funcion de evaluacion a varios pasos hasta quedar bloqueada.
evals :: (Mem,BabyC) -> (Mem,BabyC)
evals (memoria,expresion) = case expresion of 
                                      (Void) -> eval1 (memoria,expresion)
                                      (Var y1) -> eval1 (memoria,expresion)
                                      (VNum y1) -> eval1 (memoria,expresion)
                                      (VBool y1) -> eval1 (memoria,expresion)
                                      (L x1) -> eval1 (memoria,expresion)
                                      _ -> let aux = eval1 (memoria,expresion) in evals (aux)
 
-- | interp. Funcion que ejecuta un programa en una memoria vacia.
interp :: BabyC -> BabyC
interp expresion = snd (evals ([],expresion))

--Ejemplos:

-- Programa de ciclo infinito
cicloInfinito = While (VBool True) Void

-- Programa de factorial, que recibe un valor numerico
fact n = Let "x" (Ref n) (
  Let "y" (Ref (VNum 1)) (
      Seq (
          While (Gt (Deref (Var "x")) (VNum 0))
           (Seq
            (Asig (Var "y") (Prod (Deref (Var "x")) (Deref (Var "y"))))
            (Asig (Var "x") (Resta (Deref (Var "x")) (VNum 1)))
           )
          )
      (Deref (Var "y"))))
      
sumatoria n = Let "x" (Ref n) (
  Let "y" (Ref (VNum 0)) (
      Seq (
          While (Gt (Deref (Var "x")) (VNum 0))
           (Seq
            (Asig (Var "y") (Suma (Deref (Var "x")) (Deref (Var "y"))))
            (Asig (Var "x") (Resta (Deref (Var "x")) (VNum 1)))
           )
          )
      (Deref (Var "y"))))     
      

      
