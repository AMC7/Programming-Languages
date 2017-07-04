{-
- Lenguajes de programacion
- 2017-1
- Favio E. Miranda Perea
- Susana H. Martin Lunas
- Fernando A. Galicia Mendoza

- Implementacion de EAB con pares
-}

module EABP where

import Data.List

{-
- Sintaxis abstracta de las expresiones artimetico booleanas
-}

-- | Tipo que representa los nombres de variable.
type Nombre = String

-- | Tipo que representa la sintaxis abstracta para EABP.
data EABP = Var Nombre | VNum Int | VBool Bool | Suma EABP EABP | Prod EABP EABP | Suc EABP | Pred EABP | If EABP EABP EABP | IZ EABP  |
		   Let Nombre EABP EABP | Par EABP EABP | Fst EABP | Snd EABP | Or EABP EABP | Neg EABP deriving (Eq)

-- | Tipo que define una sustitucion.
type Sustitucion = (String,EABP)

-- | Tipo para los tipos EABP.
data Tipo = Nat | Boolean | Pair Tipo Tipo deriving(Show,Eq)

-- | Tipo que define el par de variables y tipos.
type Par = (Nombre,Tipo)

-- | Tipo que define un contexto.
type Ctx = [Par]


toString :: EABP -> String
toString (Var x) = x
toString (VNum x) = show x
toString (VBool x) =  show x
toString (Suma x y) = "("++(toString x)++"+"++(toString y)++")"
toString (Prod x y) = "("++(toString x)++"*"++(toString y)++")"
toString (Suc x) = "s("++(toString x)++")"
toString (Pred x) = "p("++(toString x)++")"
toString (If x y z) = "if "++(toString x)++" then "++(toString y)++" else "++(toString z)++" endif"
toString (IZ x) = "iszero("++(toString x)++")"
toString (Let x y z) = "let "++x++" = "++(toString y)++" in "++(toString z)++" end"
toString (Par x y) = "("++(toString x)++" , "++(toString y)++")"
toString (Fst x) = "fst("++(toString x)++")"
toString (Snd x) = "snd("++(toString x)++")"
toString (Or x y) = "("++(toString x)++" v "++(toString y)++")"
toString (Neg x) = "~ "++(toString x)


-- | Instancia de la clase Show para EABP.
instance Show EABP where
 show x = toString x

{-
- Implementación de la semantica dinamica
-}

-- | fv. Funcion que obtiene las variables libres de una expresion.
--
-- --> fv (Suma (Var "x") (VNum 2)) = ["x"]
-- --> fv (Let "x" (VNum 1) (Var "x")) = []
-- --> fv (Par (Var "x") (Var "y")) = ["x","y"]
fv :: EABP -> [Nombre]
fv (Var x) = [x]
fv (VNum x) = []
fv (VBool x) = []
fv (Suma x y) = union (fv x) (fv y)
fv (Prod x y) = union (fv x) (fv y)
fv (Suc x) = fv x
fv (Pred x) = fv x
fv (If x y z) = union (fv x) (union (fv y) (fv z))
fv (IZ x) = (fv x)
fv (Let x y z) = [w | w <- fv (Suma y z), w /= x]
fv (Par x y) = union (fv x) (fv y)
fv (Fst x) = fv x
fv (Snd x) = fv x
fv (Or x y) = union (fv x) (fv y)

-- | sustituye. Funcion que realiza una sustitucion.
--
-- --> sustituye (Prod (VNum 5) (Suma (Var "x") (VNum 7))) ("x",Prod (VNum 2) (Var "y")) = Prod (Prod (VNum 2) (Var "y")) (Suma (Var "x") (VNum 7))
-- --> sustituye (Let "y" (VNum 5) (Prod (Var "y") (Suma (Var "x") (VNum 7)))) ("x",Prod (VNum 2) (Var "y")) = "No es posible realizar la sustitucion."
-- --> sustituye (Par (Suma (VNum 2) (Var "x")) (VNum 2)) ("x",VNum 0) = Par (Suma (VNum 2) (VNum 0)) (VNum 2)
sustituye :: EABP -> Sustitucion -> EABP
sustituye (Let x y z) (w, e) = if elem x (union [w] (fv e)) then error "No se puede hacer la sustitucion" else (Let x (sustex y (w, e)) (sustex z (w, e)))
sustituye x s = sustex x s

sustex :: EABP -> Sustitucion -> EABP
sustex (Var x) (y, e) = if x == y then e else (Var x)
sustex (Suma x y) (z, e) = (Suma (sustex x (z, e) ) (sustex y (z, e) ))
sustex (Prod x y) (z, e) = (Prod (sustex x (z, e) ) (sustex y (z, e) ))
sustex (Or x y) (z, e) = (Or (sustex x (z, e) ) (sustex y (z, e) ))
sustex (Suc x) (y, e) = (Suc (sustex x (y, e) ))
sustex (Pred x) (y, e) = (Pred (sustex x (y, e) ))
sustex (IZ x) (y, e) = (IZ (sustex x (y, e) ))
sustex (Neg x) (y, e) = (Neg (sustex x (y, e) ))
sustex (Fst x) (y, e) = (Fst (sustex x (y, e) ))
sustex (Snd x) (y, e) = (Snd (sustex x (y, e) ))
sustex (Par x z) (y, e) = (Par (sustex x (y, e) ) (sustex z (y, e) ))
sustex (If x y z) (w, e) = (If (sustex x (w, e) ) (sustex y (w, e) ) (sustex z (w, e) ))
sustex (Let x y z) (w, e) = (sustituye (Let x y z) (w, e) )
sustex x (y, e) = x

-- | eval1. Funcion que aplica la transicion de paso pequenio a una expresion AB.
--
-- --> eval1 (Suma (VNum 1) (VNum 2)) = VNum 3
-- --> eval1 (Let "x" (VNum 1) (Suma (Var "x") (VNum 2))) = Suma (VNum 1) (VNum 2)
-- --> eval1 (Let "x" (VNum 0) (Par (Suma (VNum 2) (Var "x")) (VNum 2))) = Par (Suma (VNum 2) (VNum 0)) (VNum 2)
eval1 :: EABP -> EABP
eval1 (Var n) = (Var n)
eval1 (VNum n)  = (VNum n)
eval1 (VBool u) = (VBool u)
eval1 (Suma (VNum x)(VNum y)) = (VNum (x+y))
eval1 (Suma (VNum x) t2) = (Suma (VNum x) (eval1 t2))
eval1 (Suma t1 t2) =  ((Suma (eval1 t1) t2))
eval1 (Prod (VNum x)(VNum y)) = (VNum (x*y))
eval1 (Prod (VNum x) t2) = (Prod (VNum x) (eval1 t2))
eval1 (Prod t1 t2) =  ((Prod (eval1 t1) t2))
eval1 (Suc (VNum n))= VNum (n+1)
eval1 (Suc t1) = (Suc (eval1 t1))
eval1 (Pred (VNum n))= VNum (n-1)
eval1 (Pred t1) =  (Pred (eval1 t1))
eval1 (If (VBool x) t2 t3) 
                 |x==True = t2 
                 |otherwise = t3
						  
eval1 (If t1 t2 t3) = If (eval1 t1) t2 t3

eval1 (IZ (VNum n)) 
             |n==0 = VBool True
             |otherwise =VBool False
					
eval1 (IZ t1) = (IZ (eval1 (t1)))
eval1 (Let nombre eabp eabp1)= (sustituye eabp1 (nombre,eabp))
eval1 (Par (VNum t1) t2) = ((Par (VNum(t1)) (eval1(t2))))
eval1 (Par (VBool t1) t2) =  ((Par (VBool(t1)) (eval1(t2))))
eval1 (Par (Par t1 t2) t3) = (Par(Par (eval1 t1)(eval1 t2)) (eval (t3)))
eval1 (Par (t1)(t2)) = ((Par (eval1(t1)) (t2)))	
eval1 (Or (VBool x)(VBool y)) = (VBool (x||y))
eval1 (Or (VBool x) t2) =  (Or (VBool x) (eval1 t2))
eval1 (Or t1 t2) =  (Or (eval1 t1) t2)		
eval1 (Fst (Par (t1) _ )) =  t1
eval1 (Snd (Par _ (t2))) =  t2
eval1 x =x


-- | evals. Funcion que aplica la transicion de paso paquenio a una expresion AB, hasta que se
-- bloquee.
--
-- --> evals (Let "x"x (Suma (VNum 1) (VNum 2)) (IZ (Var "x"))) = VBool False
-- --> evals (Suma (Prod (VNum 2) (VNum 6)) (VBool True)) = Suma (VNum 12) (VBool False)
-- --> evals (Let "x" (VNum 0) (Par (Suma (VNum 2) (Var "x")) (VNum 2))) = Par (VNum 2) (VNum 2)
evals :: EABP -> EABP
evals x 
		|esValor(x) = x	
		|estaBloqueado(x) = x
		|otherwise =evals (eval1 x)	
    

	
esValor ::EABP -> Bool
esValor (VNum x) = True 
esValor (VBool x)= True
esValor (Par x y)
				 |(esValor x) && (esValor y) = True
				 |otherwise = False
esValor _ = False	

--ev= evals (Suma(Prod (VNum 1)(VNum 2)) VNum 4)

-- | eval. Funcion que aplica la transicion de paso pequenio a una expresion AB, donde
-- manda error de ejecucion en caso de no terminar con un valor.
--
-- --> eval (Let "x" (Suma (VNum 1) (VNum 2)) (IZ (Var "x"))) = "No termina con valor."
-- --> eval (Suma (Prod (VNum 2) (VNum 6)) (VBool True)) = Suma (VNum 12) (VBool False)
-- --> eval (Or (IZ (Suma (VNum 0) (VNum 0))) (IZ (VNum 1))) = VBool True
eval :: EABP -> EABP
eval x = if (esValor (evals x)) then (evals x) else error "No termina con valor" 

{-
- Implementacion de la semantica estatica
-}
tipos = [Nat, Boolean] :: [Tipo]
-- | vt. Funcion que verifica el tipado de una expresion.
--
-- --> vt [("x",Boolean)] (If (VBool True) (VBool False) (Var "x")) Boolean = True
-- --> vt [] (Let "x" (Suma (VNum 1) (VNum 2)) (IZ (Prod (Suma (Var "x") (VNum 5)) (Suma (Var "x") (VNum 2))))) Bool = True
-- --> vt [] (Par (VBool True) (Suma (VNum 2) (VNum 0))) (Pair Boolean Boolean) = False
-- --> vt [] (Par (Par (VNum 1) (VBool True)) (VNum 2)) (Pair (Pair Nat Boolean) Nat) = True
vt :: Ctx -> EABP -> Tipo -> Bool
vt c e t = if u == Nothing then False else  (elimi u) == t where u = (getTipo c e)

estaBloqueado::EABP->Bool
estaBloqueado x= if (x==eval1(x)) then True else False

isTipo :: Tipo -> Bool
isTipo Nat = True
isTipo Boolean = True
isTipo (Pair x y) = (isTipo x) && (isTipo y)

getTipo :: Ctx -> EABP -> Maybe Tipo
getTipo c (VNum x) = Just Nat
getTipo c (VBool x) = Just Boolean
getTipo c (Var x) = if t == [] then Nothing else Just (snd (head t)) where t = [r | r <- c, (fst r) == x]
getTipo c (Suma x y) = if ((getTipo c x) == Just Nat) && ((getTipo c y) == Just Nat) then Just Nat else Nothing
getTipo c (Prod x y) = if ((getTipo c x) == Just Nat) && ((getTipo c y) == Just Nat) then Just Nat else Nothing
getTipo c (Suc x) = if (getTipo c x) == Just Nat then Just Nat else Nothing
getTipo c (Pred x) = if (getTipo c x) == Just Nat then Just Nat else Nothing
getTipo c (IZ x) = if (getTipo c x) == Just Nat then Just Boolean else Nothing
getTipo c (If x y z) = if ((getTipo c x) == Just Boolean) && ((getTipo c y) == (getTipo c z)) then (getTipo c y) else Nothing
getTipo c (Or x y) = if ((getTipo c x) == Just Boolean) && ((getTipo c y) == Just Boolean) then Just Boolean else Nothing
getTipo c (Neg x) = if (getTipo c x) == Just Boolean then Just Boolean else Nothing
getTipo c (Par x y) = if (u == Nothing) || (w == Nothing) then Nothing else Just (Pair (elimi u) (elimi w)) where (u, w) = ((getTipo c x), (getTipo c y))
getTipo c (Fst (Par x y)) = getTipo c x
getTipo c (Snd (Par x y)) = getTipo c y
getTipo c (Let x y z) = if (u == Nothing) then Nothing else (getTipo (union c [(x, (elimi u))]) z) where u = (getTipo c y)
getTipo _ _ = Nothing

elimi :: Maybe Tipo -> Tipo
elimi (Just a) = a