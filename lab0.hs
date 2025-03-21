--La intención de este documento y sus ejercicios es repasar brevemente algunos
--conceptos del paradigma funcional (en haskell) que sean utiles para luego
--realizar el lab1 tales como funciones recursivas, declaración de tipos, 
--alto orden, polimorfismo, etc.

--Comenzamos repasando la estructura presente en la definición de una función
--recursiva:

factorial :: Int -> Int              --signatura de la función (declara cantidad
                                     --y tipo de cada input/output de la
                                     --función)
factorial 0 = 1                      --case base (pattern-matching con "0")
factorial n = n * factorial (n-1)    --caso recursivo (pattern-matching con "n!=0")


--Repasemos funciones recursivas sobre listas.  Podemos caracterizar (al menos)
--tres tipos de funciones recursivas sobre listas: "MAP", "FILTER", "FOLD".

--Funciones recursivas del tipo "MAP":
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x : xs) = (2*x) : duplica xs

--Las funciones del tipo "map" consisten en aplicar una función concreta a cada elemento
--de la lista, en el ejemplo de la función "duplica", la función sería f(x) = 2*x

--Otro ejemplo de función de tipo "map" es:
mas1 :: [Int] -> [Int]
mas1 [] = []
mas1 (x : xs) = (x+1) : mas1 xs

--Parece que "duplica" y "mas1" son funciones muy similares.
--Solo se diferencian en la función que aplican a cada elemento de la lista,
--mientras una aplica 2*x, la otra aplica x+1.
--Por lo tanto, surge la pregunta si podemos generalizar la funciones 
--del tipo "map", de forma tal que, "duplica" y "mas1" (e inclusive cualquier otra)
--sean un caso particular de dicha generalizacion.

--Para lograr esto utilizaremos el concepto de "ALTO ORDEN" que consiste en permitir
--que una función pueda tomar como argumento de entrada o de salida una función propiamente dicha.
--Es decir, una función es un valor posible de nuestro lenguaje tan naturalmente como los es
--un Int, o un Float, o un String, etc. 
 
--Entonces, la función que generaliza puede ser definida tomando como argumento la 
--función que es aplicada a cada elemento de la lista. 

generalMap:: [Int] -> (Int -> Int) ->  [Int]
generalMap [] f = []
generalMap (x:xs) f = f x : generalMap xs f

--Notar que "generalMap" claramente utiliza alto orden.
--Toma una lista de enteros como primer argumento,
--una función como segundo argumento (que toma un entero y devuelve un entero) 
--y devuelve como output una lista de enteros.


--Más aún, podemos generalizar el tipo de la lista [Int]
--a traves del "POLIMORFISMO" que consiste en definir funciones que
--estén bien definidas para múltiples tipo de datos.
--Asi podemos dar una versión polimórfica de la función "generalMap" de
--la siguiente manera:

polGeneralMap :: [a] -> (a -> a) -> [a]
polGeneralMap [] f = []
polGeneralMap (x:xs) f = f x : polGeneralMap xs f

--De esta manera, podemos utilizar "polGeneralMap" para realizar cualquier mapeo
--independientemente del tipo de la lista.
--De hecho, podemos dar una versión aún más polimórfica observando que el dominio y la imágen
--de la función de mapeo no necesariamente deben ser del mismo tipo. Es decir:

morePolGeneralMap :: [a] -> (a -> b) -> [b]
morePolGeneralMap [] f = []
morePolGeneralMap (x:xs) f = f x : morePolGeneralMap xs f


--EJERCICIO 1: 
--a) redefinir la función "duplica" y "mas1" en términos de "generalMap" y "polGeneralMap".
--b) definir la función esPar:: [Int] -> [Bool] en términos de "morePolGeneralMap". 
--Donde la función "esPar" mapea cada elemento de la lista a un booleano que indica si el mismo es un numero par. 
--Por ejemplo, esPar [2,9,4,5] = [True, False, True,False]. 

-- a)

auxiliarDuplica :: Num a => a -> a
auxiliarDuplica x = x * 2

generalDuplica :: [Int] -> [Int]
generalDuplica xs = generalMap xs auxiliarDuplica 

polGeneralDuplica :: Num a => [a] -> [a]
polGeneralDuplica xs = polGeneralMap xs auxiliarDuplica

auxiliarMas1 :: Num a => a -> a
auxiliarMas1 x = x + 1

generalMas1 :: [Int] -> [Int]
generalMas1 xs = generalMap xs auxiliarMas1 

polGeneralMas1 :: Num a => [a] -> [a]
polGeneralMas1 xs = polGeneralMap xs auxiliarMas1

-- b)

auxiliarEsPar :: Integral a => a -> Bool
auxiliarEsPar x = x `mod` 2 == 0

morePolGeneralEsPar :: Integral a => [a] -> [Bool]
morePolGeneralEsPar xs = morePolGeneralMap xs auxiliarEsPar

--Funciones recursivas del tipo "FILTER":
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | mod x 2 == 0 = x : soloPares xs
                 | mod x 2 /= 0 = soloPares xs
                 
--Las funciones del tipo "FILTER" consisten en filtrar los elementos de una lista sujeto
--al cumplimiento de una condición booleana, en este ejemplo, la condición sería "x es un
--número par".

--EJERCICIO 2: 
--a) generalizar la funciones de tipo "FILTER" sobre lista de enteros.
--b) dar una versión polimorfica de la misma.
--c) redefinir la función "soloPares" en términos de dicha generalización.

-- a)

generalFilter :: [Int] -> (Int -> Bool) -> [Int]
generalFilter [] f = []
generalFilter (x:xs) f
  | f x == True = x : generalFilter xs f
  | f x == False = generalFilter xs f

-- b)

polGeneralFilter :: [a] -> (a -> Bool) -> [a]
polGeneralFilter [] f = []
polGeneralFilter (x:xs) f
  | f x == True = x : polGeneralFilter xs f
  | f x == False = polGeneralFilter xs f

-- c)

auxiliarSoloPares :: Integral a => a -> Bool
auxiliarSoloPares x = x `mod` 2 == 0

generalSoloPares :: [Int] -> [Int]
generalSoloPares xs = generalFilter xs auxiliarSoloPares

polGeneralSoloPares :: Integral a => [a] -> [a]
polGeneralSoloPares xs = polGeneralFilter xs auxiliarSoloPares

--Funciones recursivas del tipo "FOLD":
sumatoria :: [Int] -> Int          
sumatoria [] = 0                  
sumatoria (x:xs) = x + sumatoria (xs)   
                 
--Las funciones del tipo "fold" se caracterizan por calcular un valor (via una función) en base a todos los elementos de una
--lista, en este ejemplo, la función sería f(x,y) = x + y.

--EJERCICIO 3: 
--a) generalizar la funciones de tipo "FOLD" sobre lista de enteros.
--b) dar una versión polimorfica de la misma.
--c) redefinir la función "sumatoria" en términos de dicha generalización.

-- a)

generalFold :: [Int] -> (Int -> Int) -> Int
generalFold [] f = 0
generalFold (x:xs) f = f x + generalFold xs f

-- b)

polGeneralFold :: Num a => [a] -> (a -> a) -> a
polGeneralFold [] f = 0
polGeneralFold (x:xs) f = f x + polGeneralFold xs f

-- c)

auxiliarSumatoria :: Num a => a -> a
auxiliarSumatoria x = x

generalSumatoria :: [Int] -> Int
generalSumatoria xs = generalFold xs auxiliarSumatoria

polGeneralSumatoria :: Num a => [a] -> a
polGeneralSumatoria xs = polGeneralFold xs auxiliarSumatoria

--Otro concepto interesante del paradigma funcional, es que, podemos definir 
--nuestos propios tipos:

type Radio = Float   --Define un "alias de tipo" (sinónimo)
type Lado = Float

--Vamos a definir 4 figuras
data Figura = Circulo Radio        --Cada uno de estos es un _constructor_
            | Cuadrado Lado        --define el constructor de un "Cuadrado"
            | Rectangulo Lado Lado --define el constructor de un "Rectangulo"
            | Punto                --define el constructor de un "Punto"
              deriving (Eq, Show)
              
--(esta última linea permite hacer que se impriman en pantalla los constructores
--de una Figura, y que se puedan comparar.)

--Y obviamente, podemos definir funciones sobre nuestros propios tipos de datos:

perimetro :: Figura -> Float
perimetro (Circulo radio) = 2 * pi * radio
perimetro (Cuadrado lado) = 4 * lado
perimetro (Rectangulo ancho alto) = 2 * ancho + 2 * alto
perimetro (Punto) = error "no se puede calcular el perimetro del punto"

--EJERCICIO 4: definir una función que devuelva la superficie de una "Figura"

superficie :: Figura -> Float
superficie (Circulo radio) = pi * (radio ** 2)
superficie (Cuadrado lado) = lado ** 2
superficie (Rectangulo ancho alto) = ancho * alto
superficie (Punto) = error "no se puede calcular la superficie del punto"

--EJERCICIO 5:

--a) Asi como definimos el tipo "Figura" en el ejercicio anterior, ahora queremos definir
--un tipo "Expr" que permita representar una expresión aritmética sobre numeros enteros
--con nuestros propios constructores: suma, producto, resta, division. 
--De esta manera, por ejemplo, la expresión "Resta (Producto (Suma 5 3) 2) 6" es una "Expr" válida.
--Definir el tipo "Expr".

type Numero = Int

data Expr = Base Numero
          | Suma Expr Expr 
          | Resta Expr Expr 
          | Producto Expr Expr 
          | Division Expr Expr 
          deriving (Eq, Show)

instance Num Expr where
 fromInteger x = Base (fromInteger x)
 (+) = Suma
 (-) = Resta
 (*) = Producto
 abs (Base x) = Base (abs x)
 signum (Base x) = Base (signum x)

--b)Luego, definir la semántica del tipo "Expr", i.e., definir una función que evalúa (en forma
--natural) una expresión aritmética "Expr". Por ejemplo: 

--evaluar (Resta (Producto (Suma 5 3) 2) 6) = 10

evaluar :: Expr -> Numero
evaluar (Base x) = x
evaluar (Suma x y) = evaluar x + evaluar y
evaluar (Resta x y) = evaluar x - evaluar y
evaluar (Producto x y) = evaluar x * evaluar y
evaluar (Division x y) = evaluar x `div` evaluar y

--EJERCICIO 6:

--a) Definir un tipo "BinTree" que permita representar un arbol binario
--polimorfico (i.e, en cuyos nodos se almacenen valores de tipo generico "a").

data BinTree a = Vacio
               | Rama (BinTree a) a (BinTree a)

--b) Definir una función recursiva que devuelva la profundidad de un "BinTree". 

profundidad :: BinTree a -> Int
profundidad (Vacio) = 0
profundidad (Rama izq _ der) = 1 + max (profundidad izq) (profundidad der)

--c) Definir una funcion general de fold que opera sobre un "BinTree" y luego, redefinir la funcion 
--de profundidad del item b en términos de esta última. 

fold :: Num a => BinTree a -> (a -> a) -> a 
fold (Vacio) f = 0
fold (Rama izq k der) f = f k + (fold izq f) + (fold der f) 

_profundidad :: BinTree a -> Int
_profundidad (Vacio) = 0
_profundidad (Rama izq _ der) = 

-- profoldidad :: BinTree a -> Int
-- profoldidad tree = fold tree (1) 

--d) Definir una función recuriva que devuelve una lista con los elementos del BinTree, y luego,
-- redefinirla en términos de la función fold del item c.

