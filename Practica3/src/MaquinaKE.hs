module MaquinaKE where

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
         | Error
         | Catch EAB EAB

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
  show Error = "ERROR"
  show (Catch e1 e2) = "catch("++show e1++","++show e2++")"

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
           | MCatch Pila EAB

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
  show (MCatch p e) = "catch("++show p++","++show e++")"

-- | Pila. Una lista de Marcos
type Pila = [Marco]

-- | Estado. Tipo que define el estado de una pila, donde:
-- Ev (Pc,Pm,e) corresponde a la evaluacion de e siendo Pc la pila actual de control
-- Dv (Pc,Pm,v) corresponde a devolver el valor v sieno Pc la pila actual de control
-- Pr (Pc,Pm,e) corresponde a propagar el error siendo Pm la pila actual de manejadores
data Estado = Ev (Pila,Pila,EAB) | Dv (Pila,Pila,EAB) | Pr (Pila,Pila,EAB)

-- | Instancia de la clase Show para la pila
instance Show Estado where
  show (Ev (pc,pm,e)) = show (pc,pm)++" > "++show e
  show (Dv (pc,pm,e)) = show (pc,pm)++" < "++show e
  show (Pr (pc,pm,e)) = show (pc,pm)++" << "++show e

-- | Tipo para los tipos EABP.
data Tipo = Nat | Boolean deriving(Show,Eq)

-- | Tipo que define el par de variables y tipos.
type Par = (Nombre,Tipo)

-- | Tipo que define un contexto.
type Ctx = [Par]

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
fv Error = []
fv (Catch x y) = union (fv x) (fv y)

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
sustituye Error s = Error
sustituye (Catch x y) s = Catch (sustituye x s) (sustituye y s)

-- | eval. Funcion que hace un paso de la transicion de la maquina K
eval :: Estado -> Estado
eval (Ev (p1, p2, VNum x)) = Dv (p1, p2, VNum x)
eval (Ev (p1, p2, VBool x)) = Dv (p1, p2, VBool x)
eval (Ev (p1, p2, Error)) = Pr (p1, p2, Error)
eval (Pr (p1, [], Error)) = Dv ([], [], Error)
eval (Pr (_, ((MCatch p e):xs), Error)) = Ev (p, xs, e)
eval (Ev (p1, p2, Catch x y)) = Ev ((MCatch [] y):p1, (MCatch p1 y):p2, x)
eval (Dv ((MCatch _ _):p1, (MCatch _ _):p2, x)) = Dv (p1, p2, x)
eval (Ev (p1, p2, Suma x y)) = Ev ((MSumaI () y):p1, p2, x)
eval (Dv ((MSumaI () y):p1, p2, VNum x)) = Ev ((MSumaD () (VNum x)):p1, p2, y)
eval (Dv ((MSumaD () (VNum x)):p1, p2, VNum y)) = Ev (p1, p2, VNum (x+y))
eval (Ev (p1, p2, Div x y)) = Ev ((MDivI () y):p1, p2, x)
eval (Dv ((MDivI () y):p1, p2, VNum x)) = Ev ((MDivD () (VNum x)):p1, p2, y)
eval (Dv ((MDivD () (VNum x)):p1, p2, VNum 0)) = Ev (p1, p2, Error)
eval (Dv ((MDivD () (VNum x)):p1, p2, VNum y)) = Ev (p1, p2, VNum (div x y))
eval (Ev (p1, p2, If x y z)) = Ev ((MIf () y z):p1, p2, x)
eval (Dv ((MIf () x y):p1, p2, VBool True)) = Ev (p1, p2, x)
eval (Dv ((MIf () x y):p1, p2, VBool False)) = Ev (p1, p2, y)
eval (Ev (p1, p2, IZ x)) = Ev ((MIZ ()):p1, p2, x)
eval (Dv ((MIZ ()):p1, p2, VNum x)) = Dv (p1, p2, VBool (x==0))
eval (Ev (p1, p2, Let x y z)) = Ev ((MLet () x z):p1, p2, y)
eval (Dv ((MLet () x z):p1, p2, y)) = Ev (p1, p2, sustituye z (x, y))
eval (Ev (p1, p2, Eq x y)) = Ev ((MEqI () y):p1, p2, x)
eval (Dv ((MEqI () y):p1, p2, x)) = Ev ((MEqD () x):p1, p2, y)
eval (Dv ((MEqD () (VNum x)):p1, p2, VNum y)) = Dv (p1, p2, VBool (x==y))
eval (Dv ((MEqD () (VBool x)):p1, p2, VBool y)) = Dv (p1, p2, VBool (x==y))
eval (Ev (p1, p2, Neg x)) = Ev ((MNeg ()):p1, p2, x)
eval (Dv ((MNeg ()):p1, p2, VBool b)) = Dv (p1, p2, VBool (not b))
eval (Ev (p1, p2, Suc x)) = Ev ((MSuc ()):p1, p2, x)
eval (Dv ((MSuc ()):p1, p2, VNum x)) = Dv (p1, p2, VNum (x+1))
eval (Ev (p1, p2, _)) = Pr (p1, p2, Error)
eval (Dv (p1, p2, _)) = Pr (p1, p2, Error)


-- | evals. Funcion que hace varios pasos de la transicion de la maquina K
evals :: Estado -> Estado
evals x = case x of
  (Dv ([], [], v)) -> x
  e -> evals $ eval x

-- | interp. Funcion que evalua una expresion EAB con catch y pilas vacias
interp :: EAB -> EAB
interp x = ge $ evals $ Ev ([], [], x)

ge :: Estado -> EAB
ge (Dv (_, _, x)) = x
ge (Ev (_, _, x)) = x
ge (Pr (_, _, x)) = x

-- | vt. Funcion que verifica el tipado de una expresion.
vt :: Ctx -> EAB -> Tipo -> Bool
vt c Error t = True
vt c (Var x) t = elem (x,t) c
vt c (VNum x) t = t == Nat
vt c (VBool x) t = t == Boolean
vt c (Suma x y) Nat = (vt c x Nat) && (vt c y Nat)
vt c (Div x y) Nat = (vt c x Nat) && (vt c y Nat)
vt c (If x y z) t = (vt c x Boolean) && (vt c y t) && (vt c z t)
vt c (IZ x) Boolean = vt c x Nat
vt c (Let x y z) t = vt c (sustituye z (x, y)) t
vt c (Eq x y) Boolean = ((vt c x Nat) && (vt c y Nat)) || ((vt c x Boolean) && (vt c y Boolean))
vt c (Neg x) Boolean = vt c x Boolean
vt c (Suc x) Nat = vt c x Nat
vt c (Catch x y) t = (vt c x t) && (vt c y t)
vt _ _ _ = False

qj :: Maybe a -> a
qj (Just t) = t

--Ejemplos

ejem1 n = interp (IZ n)
--Resultados:
--Si se da un valor numerico, la ejecucion es exitosa
--Si se da un valor booleano, lanza error

ejem2 n1 n2 = interp (Suma n1 n2)
--Resultados:
--Si se dan un valores numericos, la ejecucion es exitosa
--Si alguno de los parametros son no numericos, lanza error

ejem3 n1 n2 = interp (Div n1 n2)
--Resultados:
--Si se dan un valores numericos, la ejecucion es exitosa
--Si alguno de los parametros son no numericos o el segundo parametro es igual a 0, lanza error

ejem4 n1 n2 = interp (Eq n1 n2)
--Resultados:
--Si los parametros son valores del mismo tipo, la ejecucion es exitosa
--Si los parametros no son valores del mismo tipo, lanza error

ejem5 g e1 e2 = interp (If g e1 e2)
--Resultados:
--Si la guardia es booleana, la ejecucion es exitosa
--Si la guarda no es booleana, lanza error

ejem6 n = interp (If (Neg (IZ n)) (VBool True) (VBool False))
--Resultados:
--Si n = 0, entonces devuelve False
--Si n <> 0, entonces devuelve True
--Si n no es un número, devuelve error

ejem7 = vt [] (Suma (VNum 1) (Suc Error)) Nat
--Resultado: True

ejem8 = vt [] (Catch (If (VBool True) (VNum 1) (VNum 2)) Error) Nat
--Resultado: True

ejem9 = vt [] (Div (VNum 1) (VBool True)) Nat
--Resultado: False

ejem10 = vt [] (Catch (Let "x" (VNum 1) (Suma (Var "x") (VBool True))) Error) Boolean
--Resultado: False

ejem11 = vt [] (Catch (Let "x" (VNum 1) (Suma (Var "x") (VBool True))) Error) Nat
--Resultado:False

ejem12 = vt [] (Catch (Let "x" (VNum 1) (Suma (Var "x") (VNum 1))) Error) Nat
--Resultado:True
