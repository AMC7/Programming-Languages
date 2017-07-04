{-
- Lenguajes de programacion
- 2017-1
- Favio E. Miranda Perea
- Susana H. Martin Lunas
- Fernando A. Galicia Mendoza
- Implementacion de lambda sin tipos
.-}

module LambdaPuro where

import Data.List

-- | Nombre. Tipo que define un nombre de variables como una cadena de texto.
type Nombre = String

-- | ExpL. Tipo que representa una expresion lambda sin tipos.
data ExpL = Var Nombre | L Nombre ExpL | App ExpL ExpL deriving(Eq)

-- | Reducto. Tipo que representa un reducto del calculo lambda.
type Reducto = (Nombre,ExpL)

-- | Instancia de la clase Show.
instance Show ExpL where
  show = toString   
{-
- Lambda nativo de Haskell
-}

toString::ExpL->String
toString (Var x) = x
toString (L x y) ="\\"++ x ++ "."++toString y
toString (App x y) = "(" ++ (toString x)++"  "++ (toString y) ++ ")"

-- | true. Funcion que representa la constante verdadera, en calculo lambda.
true = \ x -> \y ->x

-- | false. Funcion que representa la constante falsa, en calculo lambda.
false = \x -> \y ->y

-- | nott. Funcion que representa el operador de negacion, en calculo lambda.
nott = \z -> z false true
 
-- | andL. Funcion que representa el operador de conjuncion, en calculo lambda.
andL = \x -> \y ->x y false

-- | orL. Funcion que representa el operador de disyuncion, en calculo lambda.
orL = \x -> \y ->x y true

-- | impL. Funcion que representa el operador de implicacion, en calculo lambda.

impL = \x -> \y -> (nott x) true y

{-
- Implementacion de lambda sin tipos en Haskell
-}

-- | fv. Funcion que devuelve las variables libres de una expresion lambda.
fv :: ExpL -> [Nombre] 
fv (Var x) = [x]
fv (L   x y) = [(s)|s <- (fv y) , s /= x]
fv (App x y ) = union (fv(x)) (fv(y))

-- | sustitucion. Funcion que implementa la funcion de sustitucion en expresiones lambda.
--
-- --> Considerar el caso de capturar variables libres.
sustitucion :: ExpL -> Reducto -> ExpL
sustitucion (Var x) (y,z) 
                         |(x==y) = z
                         |otherwise =(Var x)
sustitucion (App x y) z = (App ((sustitucion x) z) ((sustitucion y) z))
sustitucion (L x y) (w,z) 
                          |not ( elem (x) (union [w] (fv z)))= (L x (sustitucion y (w,z)))
                          |otherwise = error ((show (x))++ (show (union [w] (fv z))))
    
-- data ExpL = Var Nombre | L Nombre ExpL | App ExpL ExpL deriving(Eq)					 						 
-- | beta. Funcion que implementa un paso de la beta reduccion.
beta :: ExpL -> ExpL
beta (Var x  ) = (Var x)
beta (L x y )  = (L x (beta y))
beta (App w z) = case w of (L x y) -> (sustitucion y (x,z)) 
                           _-> (App (beta w) (beta z))


-- | betaS. Funcion que implementa n pasos de beta reducciones con n >= 0.
betaS :: ExpL -> ExpL
betaS (Var x) = beta (Var x)
betaS x = let reducto = (beta x) in if (x == reducto) then reducto else betaS reducto



{-
- Ejemplos
-}


ejemplo1P1 = andL (nott false) true True False
--Resultado: True

ejemplo2P1 = orL (andL true false) true True False
--Resultado: True

ejemplo3P1 = nott(nott true) True False
--Resultado: True

ejemplo4P1 = impL false true True False
--Resultado: True

ejemplo1P2 = sustitucion (L "x" (App (Var "x") (Var "y"))) ("y",L "z" (Var "z"))
--Resultado: (\x.(x (\z.z)))

ejemplo2P2 = sustitucion (L "x" (Var "y")) ("y",Var "x")
--Resultado: *** Exception: No se puede realizar sustitucion.

ejemplo3P2 = beta (App (L "x" (App (Var "x") (Var "y"))) (L "z" (Var "z")))
--Resultado: ((\z.z) y)

ejemplo4P2 = beta (App (App (L "x" (Var "x")) (Var "w")) (App (L "z" (Var "z")) (Var "y")))
--Resultado: (w ((\z.z) y))



cero = L  "s"  (L "z"  (Var "z"))
uno  = L  "s1" (L "z1" (App (Var "s1") (Var "z1")))
dos  = L  "s8" (L "z8" (App (Var "s8") (App (Var "s8") (Var "z8"))))
tres = L  "s7" (L "z7" (App (Var "s7") (App (Var "s7") (App (Var "s7") (Var "z7")))))

suc = L "n" (L "s2" (L "z2" (App (Var "s2") (App (App (Var "n") (Var "s2")) (Var "z2")))))
sumaL =L "m4"(L "n4"(App (App (Var "n4") (suc))	  (Var "m4"))) 

sumaTresTres = (betaS (App (App sumaL tres ) dos))
--Resultado: (\s2.(\z2.(s2 z2)))

sumaDosDos  = (betaS (App (App sumaL dos) dos))
--Resultado: (\s2.(\z2.(s2 (s2 (s2 (s2 z2)))))

prodL = L "m16" (L "n16"((App (App (Var "m16") (App sumaL (Var "n16"))) cero)))

productoTresTres = (betaS (App (App prodL cero) uno))
--Resultado: (\s.\z.z)

productoUnoCero = (betaS (App (App prodL cero) uno))
--Resultado: (\s.\z.z)

ejemplo5P2 = betaS (App suc cero)
--Resultado: (\s2.(\z2.(s2 z2)))

ejemplo6P2 = betaS (App suc uno)
--Resultado: (\s2.(\z2.(s2 (s2 z2))))

--Un bonito ejemplo
omega = L "x" (App (Var "x") (Var "x"))
ejemplo7P2 = betaS (App omega omega)
--Resultado: Se cicla el programa.
