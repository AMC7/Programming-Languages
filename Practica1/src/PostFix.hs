{-
- Lenguajes de programacion
- 2017-1
- Favio E. Miranda Perea
- Susana H. Martin Lunas
- Fernando A. Galicia Mendoza

- Implementacion de:
- Verificacion de sintaxis de un programa postfix.
- Instrucciones add y sub
-}

-- ¿consideramos al comando como una secuencia ejecutable?
module PostFix where



--Palabra reservada postfix
data PF = POSTFIX deriving(Show,Eq)

--Tipo que define un comando postfix
data Comando = L Int | EXEC | ADD | SUB | MUL | DIV | REM | Eq | Gt | Lt | SEL | NGET | POP | SWAP | SEC [Comando]  deriving(Show,Eq)

--La pila es una lista de comandos
type Pila = [Comando]

--Un programa es una tupla la cual tiene la palabra reservada postfix, un numero natural y una lista de comandos
type Programa = (PF,Int,[Comando])

{-
- Verificacion de la sintaxis de un programa	
-}

pilaConSoloNumeros:: Pila -> Bool
pilaConSoloNumeros []=True
pilaConSoloNumeros (x:xs) = case x of (L a) -> True && (pilaConSoloNumeros (xs)) 
                                      _ -> False

--Prueba use 	pilaConSoloNumeros [] ,pilaConSoloNumeros [(L 1),(L 2)] ,pilaConSoloNumeros [SEL ,(L 1),(L 2)]

--Funcion que determina que un porgrama este sintacticamente bien formado
bienFormado :: Programa -> Pila -> Bool
bienFormado (p,n,l) s = (n == length s) && (pilaConSoloNumeros s) && (prPF (p,n,l))

prPF::Programa->Bool
prPF (p,n,l) = (p==POSTFIX)&&(n>(-1))

{-
- Funciones auxiliares para la ejecucion de un programa
-}

--Funcion que realiza una operacion con dos literales enteras
--Operaciones con dos literales enteras
--
opL :: Comando -> Comando -> Comando -> Comando
opL (L x) (L y) SUB = L (x-y)
opL (L x) (L y) ADD = L (x+y)
opL (L x) (L y) MUL = L (x*y)
opL (L x) (L y) DIV = if (y==0) then error "Error de ejecucion no se puede dividir entre 0"  else L (div x y)
opL (L x) (L y) REM = if (y==0) then error "Error de ejecucion no se puede dividir entre 0" else L (mod x y)
opL (L x) (L y) Lt = if (x<y) then (L 1) else (L 0)
opL (L x) (L y) Eq = if (x==y) then (L 1) else (L 0)
opL (L x) (L y) Gt = if (x>y) then (L 1) else (L 0)
opL _ _ x= error  ((show x) ++ " es una funcion de dos argumentos enteros")


--Funcion que ejecuta el sel con una literal entera
--sel: Si el primer (tope), segundo y tercer elementos de la pila son v1; v2; v3 respectivamente, eliminarlos
--de la pila. Si v3 es el numero 0 entonces agregar v1 a la pila; si v3 es un numero distinto de
--0, agregar v2 a la pila.
selL :: Comando -> Comando -> Comando -> Comando
selL x y z= case x of (L 0) -> y 
                      _ -> z



--Funcion que ejecuta el nget con una literal entera
ngetL :: Pila -> Comando
ngetL (x:xs) = case x of (L e)-> if (e<1)||(e> (length (x:xs))-1)then error "Error de ejecucion indice invalido"   else (x:xs)!!e
                         _ -> error "Error de sintaxis el tope de la pila no es valido deberia de ser una literal"


{-
- Ejecucion de un programa
-}

--Funcion que ejecuta una secuencia dada una pila
execSec :: [Comando] -> Pila -> Pila
execSec [] p = p
execSec (c:cs) p = case c of
  (L x) -> execSec cs ((L x):p)
  SEL -> if (length p) >= 3 then execSec cs (res:p') else error "Error de ejecucion SEL: Insuficientes argumentos." where 
    p' = snd (splitAt 3 p)
    p'' = fst (splitAt 3 p)
    res =selL (p''!!2)(p''!!0)(p''!!1)
  POP -> if (length p) >= 1 then execSec cs (tail p) else error "Error de ejecucion POP: Pila Vacía"  
  SWAP -> if (length p) >=2 then execSec cs (u:d:p') else error "Error de Ejecucion SWAP: No hay suficientes argumentos" where
    p' = snd (splitAt 2 p)
    p''= fst (splitAt 2 p)
    u = p''!!1
    d = p''!!0
  NGET -> execSec cs ((ngetL p):p)
  (SEC x)-> execSec cs  (SEC x:p)
  EXEC -> case (head p) of (SEC (x:xs))-> execSec (c:cs) ((SEC xs):execSec [x] (tail p))
                           (SEC []) ->   execSec cs (tail p)
                           _->error "No es una secuencia ejecutable"
  _ ->  if (length p)>= 2 then execSec cs (res:p') else error "Error de ejecucion la operacion no tiene suficientes argumentos" where
    p'= snd (splitAt 2 p)
    p''= fst (splitAt 2 p)
    res = opL (p''!!1)(p''!!0) c
 


-- exec (POSTFIX,0,[SEC [L 7,MUL],EXEC]) ([L 2])

    
--Funcion que ejecuta un programa
exec :: Programa -> Pila -> Comando
exec (p,i,l) s = if (bienFormado (p,i,l) s) then regresaResultadoValido (execSec l s)
                         else error "Error de Sintaxis :No esta bien formado"
 
regresaResultadoValido::Pila->Comando
regresaResultadoValido [] =error  "Error de Ejecucion :No es un resultado valido la pila es vacia"
regresaResultadoValido (x:xs) =case x of (L a) -> (L a)
                                         _ ->error "El resultado debe de ser un entero"
 

--Pruebas que realice

---Ejemplos
basico=exec (POSTFIX ,0 ,[L 1]) ([])
--Resultado L 1
swape=exec (POSTFIX ,0 ,[(L 1),SWAP]) ([])
--Error
pope=exec (POSTFIX,1,[POP]) ([L 1]) 

sub=exec (POSTFIX,2,[SUB]) ([L 1,L 3]) 
less=exec (POSTFIX,2,[Lt]) ([L 1,L 3]) 
--Regresa 0

sel1 = selL (L 0) (L 1) MUL --SI
--Resultado: L 1

sel2 = selL (L 1) (L 1) MUL --SI
--Resultado: MUL
sel3 = selL (L 0) (SEC [L 1,MUL]) (L 2) --SI
--Resultado: SEC [L 1,MUL]

nget1 = ngetL [L 1,MUL,L 2,L 3]-- SI
--Resultado: MUL

nget2 = ngetL [L 3,L 1,L 2,SEC [ADD,L 1]] --SI
--Resultado: SEC[ADD,L 1]

nget3 = ngetL [MUL,L 1,L 2,SEC [ADD,L 1]]-- SI 
--Resultado: Error instruccion: No se puede ejecutar NGET

nget4 = ngetL [L 4,L 1,L 2,SEC [ADD,L 1]] -- SI
--Resultado: Error instruccion: No se puede ejecutar NGET

secuencia1 = exec (POSTFIX,0,[(L (-1)),(L 2),ADD,(L 3),MUL]) ([]) --SI
--Resultado ejecucion programa: L 3

secuencia2 = exec (POSTFIX,3,[MUL,SWAP,L 2,MUL,SWAP,SUB]) ([L 5,L 4,L 3])--SI
--Resultado ejecucion programa: L (-14)

secuencia3 =  exec (POSTFIX,1,[(SEC [L 7,ADD]),EXEC]) ([L 1])--SI
--Resultado ejecucion programa: L 8

secuencia4 = exec (POSTFIX,0,[SEC[L 0,SWAP,SUB],L 7,SWAP,EXEC]) ([])--SI
--Resultado ejecucion programa: L (-7)

secuencia5 = exec (POSTFIX,4,[Lt,SEC [ADD],SEC [MUL],SEL,EXEC]) ([L 5,L 6,L 4,L 3])--SI
--Resultado ejecucion programa: L 12

secuencia6 =exec (POSTFIX,1,[SEC [L 2,MUL],EXEC]) [(L 7)]--SI
--Resultado ejecucion programa: L 14

secuencia7= exec (POSTFIX,1,[Lt,SEC [ADD],SEC [MUL],SEL,EXEC]) ([L 5,L 6,L 4,L 3])--SI
--Devuelve: "Error de sintaxis."

--Ejemplos de la nota
ex1 = exec (POSTFIX,0,[L (-1),L 2 ,ADD ,L 3 ,MUL]) ([]) -- SI 
--Devuelve L 3
ex2 = exec (POSTFIX,1,[(SEC [L 2,MUL]),EXEC]) ([L 7])
--Devuelve L 14 
ex3 = exec (POSTFIX,2,[L 2,NGET]) ([L 9 ,L 12])
--L 12
ex4 = exec (POSTFIX,1,[SEC [L 2,MUL],NGET]) ([L 3])

