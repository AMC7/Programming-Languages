module MaquinaKC where

import Data.List

-- | Nombre. Tipo que define el nombre de una variable
type Nombre = String

-- | EAB. Tipo que representa una expresion del lenguaje imperativo EAB.
data EAB = Var Nombre
         | VNum Int
         | VBool Bool
         | IZ EAB
         | Suc EAB
         | Neg EAB
         | Suma EAB EAB
         | Div EAB EAB
         | Eq EAB EAB
         | If EAB EAB EAB
         | Let Nombre EAB EAB
         | Letcc Nombre EAB
         | Continue EAB EAB
         | Cont Pila

-- | Instancia de la clase Show para EAB
instance Show EAB where
  show (Var x) = x
  show (VNum n) = "num["++show n++"]"
  show (VBool b) = "bool["++show b++"]"
  show (IZ e) = "iz("++show e++")"
  show (Suc e) = "suc("++show e++")"
  show (Neg e) = "neg("++show e++")"
  show (Suma e1 e2) = "suma("++show e1++","++show e2++")"
  show (Div e1 e2) = "div("++show e1++","++show e2++")"
  show (Eq e1 e2) = "eq("++show e1++","++show e2++")"
  show (If e1 e2 e3) = "if("++show e1++","++show e2++","++show e3++")"
  show (Let x e1 e2) = "let("++show e1++","++x++"."++show e2++")"
  show (Letcc x e) = "letcc("++x++"."++show e++")"
  show (Continue e1 e2) = "continue("++show e1++","++show e2++")"
  show (Cont p) = "cont("++show p++")"

-- | Sust. Tipo que representa una sustitucion
type Sust = (Nombre,EAB)

-- | Hueco. Tipo que representa el lugar donde se esta llevando a cabo la evaluacion
type Hueco = ()

-- | Marco. Tipo que define un marco para la pila
data Marco = MIZ Hueco
           | MSuc Hueco
           | MNeg Hueco
           | MSumaI Hueco EAB
           | MSumaD Hueco EAB
           | MDivI Hueco EAB
           | MDivD Hueco EAB
           | MEqI Hueco EAB
           | MEqD Hueco EAB
           | MIf Hueco EAB EAB
           | MLet Hueco String EAB
           | MContinueI Hueco EAB
           | MContinueD Hueco EAB

-- | Instancia de la clase Show para Marco
instance Show Marco where
  show (MIZ ()) = "iz(-)"
  show (MNeg ()) = "neg(-)"
  show (MSuc ()) = "suc(-)"
  show (MSumaI () e) = "suma(-,"++show e++")"
  show (MSumaD () e) = "suma("++show e++",-)"
  show (MDivI () e) = "div(-,"++show e++")"
  show (MDivD () e) = "div("++show e++",-)"
  show (MEqI () e) = "eq(-,"++show e++")"
  show (MEqD () e) = "eq("++show e++",-)"
  show (MIf () e1 e2) = "if(-,"++show e1++","++show e2++")"
  show (MLet () x e) = "let(-,"++x++"."++show e++")"
  show (MContinueI () e) = "continue(-,"++show e++")"
  show (MContinueD () e) = "continue("++show e++",-)"

-- | Pila. Una lista de Marcos
type Pila = [Marco]

-- | Estado. Tipo que define el estado de una pila, donde:
-- Ev (P,e) corresponde a la evaluacion de e siendo P la pila actual de control
-- Dv (P,v) corresponde a devolver el valor v sieno P la pila actual de control
data Estado = Ev (Pila,EAB) | Dv (Pila,EAB)

-- | Instancia de la clase Show para la pila
instance Show Estado where
  show (Ev (p,e)) = show p++" > "++show e
  show (Dv (p,e)) = show p++" < "++show e

-- | fv. Funcion que devuelve las variables libres de una EAB
fv :: EAB -> [String]
fv (Var x) = [x]
fv (VNum x) = []
fv (VBool x) = []
fv (Suma x y) = union (fv x) (fv y)
fv (Div x y) = union (fv x) (fv y)
fv (If x y z) = union (fv x) (union (fv y) (fv z))
fv (IZ x) = (fv x)
fv (Let x y z) = [w | w <- fv (z), w /= x]
fv (Eq x y) = union (fv x) (fv y)
fv (Neg x) = fv x
fv (Suc x) = fv x
fv (Letcc k e) = [w | w <- (fv e), w /= k]
fv (Cont p) = []
fv (Continue k e) = union (fv k) (fv e)

-- | sustituye. Funcion que realiza la sustitucion de una EAB 
sustituye :: EAB -> Sust -> EAB
sustituye (Var x) (s, b) = if x==s then b else Var x
sustituye (VNum x) s = VNum x
sustituye (VBool x) s = VBool x
sustituye (Suma x y) s = Suma (sustituye x s) (sustituye y s)
sustituye (Div x y) s = Div (sustituye x s) (sustituye y s)
sustituye (If x y z) s = If (sustituye x s) (sustituye y s) (sustituye z s)
sustituye (IZ x) s = IZ (sustituye x s)
sustituye (Let x y z) (s, b) = if x==s then (Let x (sustituye y (s, b)) z) else (if elem x (fv b) then error (x++"en las variables libres de la sustitucion.")
  else Let x (sustituye y (s, b)) (sustituye z (s, b)))
sustituye (Eq x y) s = Eq (sustituye x s) (sustituye y s)
sustituye (Neg x) s = Neg (sustituye x s)
sustituye (Suc x) s = Suc (sustituye x s)
sustituye (Letcc k e) (s, b) = if k==s then Letcc k e else (if elem k (fv b) then error (k++"en las variables libres de la sustitucion.")
  else Letcc k (sustituye e (s, b)))
sustituye (Continue k e) s = Continue (sustituye k s) (sustituye e s)
sustituye (Cont p) s = Cont p

-- | eval. Funcion que hace un paso de la transicion de la maquina K
eval :: Estado -> Estado
eval (Ev (p1, VNum x)) = Dv (p1, VNum x)
eval (Ev (p1, VBool x)) = Dv (p1, VBool x)
eval (Ev (p, Suma x y)) = Ev ((MSumaI () y):p, x)
eval (Dv ((MSumaI () y):p, VNum x)) = Ev ((MSumaD () (VNum x)):p, y)
eval (Dv ((MSumaD () (VNum x)):p, VNum y)) = Dv (p, VNum (x+y))
eval (Ev (p, Div x y)) = Ev ((MDivI () y):p, x)
eval (Dv ((MDivI () y):p, VNum x)) = Ev ((MDivD () (VNum x)):p, y)
eval (Dv ((MDivD () (VNum x)):p, VNum y)) = Dv (p, VNum (div x y))
eval (Ev (p, If x y z)) = Ev ((MIf () y z):p, x)
eval (Dv ((MIf () y z):p, VBool True)) = Ev (p, y)
eval (Dv ((MIf () y z):p, VBool False)) = Ev (p, z)
eval (Ev (p, IZ x)) = Ev ((MIZ ()):p, x)
eval (Dv ((MIZ ()):p, VNum x)) = Dv (p, VBool (x==0))
eval (Ev (p, Let x y z)) = Ev ((MLet () x z):p, y)
eval (Dv ((MLet () x z):p, y)) = Ev (p, sustituye z (x,y))
eval (Ev (p, Eq x y)) = Ev ((MEqI () y):p, x)
eval (Dv ((MEqI () y):p, x)) = Ev ((MEqD () x):p, y)
eval (Dv ((MEqD () (VNum x)):p, VNum y)) = Dv (p, VBool (x==y))
eval (Dv ((MEqD () (VBool x)):p, VBool y)) = Dv (p, VBool (x==y))
eval (Ev (p, Neg x)) = Ev ((MNeg ()):p, x)
eval (Dv ((MNeg ()):p, VBool b)) = Dv (p, VBool (not b))
eval (Ev (p, Suc x)) = Ev ((MSuc ()):p, x)
eval (Dv ((MSuc ()):p, VNum x)) = Dv (p, VNum (x+1))
eval (Ev (p, Letcc k e)) = Ev (p, sustituye e (k, Cont p))
eval (Ev (p, Continue k e)) = Ev ((MContinueI () e):p, k)
eval (Dv ((MContinueI () e):p, k)) = Ev ((MContinueD () k):p, e)
eval (Dv ((MContinueD () (Cont p)):_, e)) = Dv (p, e)
eval (Ev (p, Cont c)) = Dv (p, Cont c) 


-- | evals. Funcion que hace varios pasos de la transicion de la maquina K
evals :: Estado -> Estado
evals x = case x of
  Dv ([], v) -> x
  e -> evals $ eval x

-- | interp. Funcion que evalua una expresion EAB con catch y pilas vacias
interp :: EAB -> EAB
interp x = ge $ evals $ Ev ([], x)

ge :: Estado -> EAB
ge (Dv (_, x)) = x
ge (Ev (_, x)) = x

--Ejemplos

ejem1 = interp (Letcc "k" (Suc (Continue (Var "k") (VNum 2))))
--Resultado: num[2]

ejem2 = interp (Letcc "k" (Suc (Continue (Var "k") (Continue (Var "k") (VNum 1)))))
--Resultado: num[1]

ejem3 = interp (Let "x" (VNum 1) (Letcc "k" (Suma (VNum 1) (Continue (Var "k") (Suma (Var "x") (VNum 1))))))
--Resultado: num[2]
