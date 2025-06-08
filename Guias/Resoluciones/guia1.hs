--1. Considerar las siguientes funciones. Tiparlas e indicar cuáles no están currificadas.
--Para las que no, currificarlas.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}



{-# HLINT ignore "Redundant lambda" #-}
max2 :: (Int, Int) -> Int --Esta asumo que es para Ints, si no debería usar Ord a
max2(x, y) | x >= y = x
           | otherwise = y

max2Currificada :: Int -> (Int -> Int)
max2Currificada x y | x >= y = x
                    | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorialCurrificada :: Float -> Float -> Float
normaVectorialCurrificada x y = sqrt (x^2 + y^2)


subtraer :: Int -> Int -> Int --Resta donde al pasar a y b, hace b-a.
subtraer = flip (-)

predecesor :: Int -> Int --Esto recibe el a, del b-a, espera el b. Y le resta uno
predecesor = subtract 1

evaluarEnCero :: (Int -> a) -> a --Le paso una función y a esta función le pasa el 0.
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> (a -> a)
dosVeces = \f -> f.f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> (a -> c)
flipRaro = flip flip


--2. Definir la función curry, que dada una función de dos argumentos, devuelve su versión currificada
curry2 :: ((a, b) -> c) -> (a -> b -> c)
curry2 f x y = f (x, y) --O sea, creo una nueva f que tenga el "valor" de "f (x, y)"

uncurry2 :: (a -> b -> c) -> ((a, b) -> c)
uncurry2 f (x, y) = f x y

--No se puede definir la función porque no se le podría dar tipado justamente al ser arbitraria la cantidad de argumentos.

--3. Esquemas de recursión. 
--I) Redefinir usando foldr:
sumRedefined :: [Int] -> Int
sumRedefined = foldr (+) 0 --La idea es arranco en 0, y voy aplicando la función suma a todos los elementos de la lista. El acumulador arranca en 0.Applicative

elemRedefined :: Eq a => a -> [a] -> Bool
elemRedefined e = foldr (\x acc -> (x == e) || acc) False

concatRedefined :: [a] -> [a] -> [a]
concatRedefined xs ys = foldr (:) ys xs

filterRedefined :: (a -> Bool) -> [a] -> [a]
filterRedefined cond = foldr (\x acc -> if cond x then x:acc else acc) []

mapRedefined :: (a -> b) -> [a] -> [b]
mapRedefined f = foldr (\x acc -> f x:acc) []

--II) Definir mejorSegun

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun cond = foldr1 (\x y -> if cond x y then x else y)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\rec x -> if null rec then [x] else rec ++ [last rec + x]) []
--Acá convenía hacerlo con foldl pues permite agarrar el primer elemento e iterar sobre los siguientes

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAltInversa :: Num a => [a] -> a
sumaAltInversa = foldr1 (-) --Arrancamos por el último elemento así hacemos primero la resta del último menos el anteúltimo.

--4)
ponerEnTodasLasPosiciones :: a -> [a] -> [[a]]
ponerEnTodasLasPosiciones x [] = [[x]]
ponerEnTodasLasPosiciones x ys = [take i ys ++ [x] ++ drop i ys | i <- [0..length ys]]


permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones [x] = [[x]]
permutaciones (x:xs) = concatMap (ponerEnTodasLasPosiciones x) (permutaciones xs)
--Para cada permutación de la cola, pongo el primer elemento en todas las posiciones posibles.
partes :: [a] -> [[a]]
partes = foldr ((\x rec -> (map (x:) rec ++ rec))) [[]]

prefijos :: [a] -> [[a]]
prefijos xs = [take i xs | i <- [0..length xs]]

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = [take i (x:xs) | i <- [1..length (x:xs)]] ++ sublistas xs

--5)Considerar las siguientes funciones
--Esta no es recursión estructural porque estamos accediendo a la cola de la lista de forma directa.
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                            then [x]
                            else x : elementosEnPosicionesPares (tail xs)
--Esta sí es recursión estructural, por lo que podemos reescribirla usando foldr.
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)

entrelazar' :: [a] -> [a] -> [a]
entrelazar' xs ys = foldr (\x rec -> if null rec then x : ys else x : head rec : tail rec) [] xs
--6. Esquema de recursión estructural
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

--a)
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna x = recr (\y ys rec -> if x == y then ys else y : rec) []
--y es la cabeza e ys es la cola, si x == y solo devuelvo la cola (sin la recursión) y si no,
--concateno y:rec para no perder la cabeza actual y vuelvo a fijarme en la recursión.

--b)
--foldr no es adecuado porque necesitamos en un momento acceder a la cola entera de la lista.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado x = recr (\y ys rec -> if x <= y then x:y:ys else y:rec) [x]


--7.Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
--listas finitas e infinitas.

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a, b)] --Equivalente de zip
armarPares = foldr (\x rec (y:ys) -> (x,y):rec ys) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c] --Equivalente de zipWith
mapDoble f xs ys = mapPares f (armarPares xs ys)--Hacer con foldr

--8.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\fila rec (ys:yss) -> zipWith (+) fila ys : rec yss) (const [])


transponer :: [[Int]] -> [[Int]]
transponer  = foldr (\fila rec -> zipWith (:) fila (rec ++ repeat [])) []

--Otras estructuras de datos
--9
--a) foldNat
foldNat :: (Int -> b -> b) -> b -> Int -> b
foldNat f z 0 = z
foldNat f z x = f x (foldNat f z (x-1))

--b) potencia
potencia :: Int -> Int -> Int
potencia base = foldNat (\potencia rec -> base * rec) 1 --Acá hago recursión estructural sobre la potencia.

genLista :: a -> (a -> a) -> Int -> [a]
genLista x f = foldNat (\_ rec -> rec ++ [f (last rec)]) [x]

desdeHasta :: Int -> Int -> [Int]
desdeHasta x y = genLista x (+1) (y-1)

--Ejercicio 11
data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

--Definamos el esquema de recursión estructural para esta estructura.
foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli cX cCte cSuma cProd polinomio = case polinomio of
                                            X -> cX
                                            Cte n -> cCte n
                                            Suma p q -> cSuma (rec p) (rec q)
                                            Prod p q -> cProd (rec p) (rec q)
                                        where rec = foldPoli cX cCte cSuma cProd

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli n id (+) (*) --Se puede usar id.



--Ejercicio 12
data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin arbol = case arbol of
                            Nil -> cNil
                            (Bin i r d ) -> cBin (rec i) r (rec d)
                            where rec = foldAB cNil cBin

recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB cNil cBin arbol = case arbol of
                        Nil -> cNil
                        (Bin i r d) -> cBin i (rec i) r d (rec d)
                        where rec = recAB cNil cBin

esNil :: AB a -> Bool
esNil arbol = case arbol of
                Nil -> True
                _ -> False

altura :: AB a -> Int
altura = foldAB 0 (\recIzq r recDer -> (max recIzq recDer) + 1)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\recIzq _ recDer -> 1 + recIzq + recDer)
--Casos de árboles binarios para testear

raizAB :: AB a -> a
raizAB (Bin _ r _) = r

arbolVariosNodos :: AB Int
arbolVariosNodos = Bin (Bin (Bin Nil 100 Nil) 1 (Bin Nil 1 (Bin Nil 1 Nil))) 1 (Bin (Bin (Bin Nil 1 Nil) 1 Nil) 1 (Bin Nil 1 Nil))

arbolMuchaAltura :: AB Int
arbolMuchaAltura = Bin (Bin (Bin (Bin (Bin (Bin (Bin Nil 1 Nil) 1 Nil) 1 Nil) 1 Nil) 1 Nil) 1 Nil) 1 Nil


mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB cond (Bin i r d) = recAB r (\i recI valor d recD -> mejorEntre valor (mejorEntre (if esNil i then valor else recI) (if esNil d then valor else recD))) (Bin i r d)
   where mejorEntre x y = if cond x y then x else y

--La idea es esta, si tengo que meterme en un caso Nil, devuelvo la raíz, si no, me fijo el mejor entre la raíz y la recursión.
--El tema es que está recursión puede ser o no sobre un Nil, y si es sobre un Nil debo evitarla, porque no voy a obtener nada de tipo a
--Para eso sirve el if.
--Uso recAB y no foldAB porque necesito saber si el hijo izquierdo o derecho es Nil, cosa que no puedo hacer con foldAB

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\i recI raiz d recD -> ((esNil i || (raiz >= raizAB i)) && recI) && ((esNil d || (raiz < raizAB d)) && recD))

--Usé recAB y no foldAB porque necesito comparar inmediatamente cada raíz con cada raíz de cada subárbol. Cosa que no
--Puedo hacer con foldAB.
ramas :: AB a -> [[a]]
ramas = foldAB [] (\recI r recD -> if null recI && null recD
                                   then [[r]] -- Caso hoja: una única rama con el valor de la raíz
                                   else map (r:) recI ++ map (r:) recD)

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura   = foldAB (const True) (\recI r recD arbolB -> case arbolB of 
   Nil -> False
   Bin iB rB dB -> recI iB && recD dB) 

arbolSencillo :: AB Int
arbolSencillo = Bin (Bin Nil 2 Nil) 1 (Bin Nil 3 Nil)

arbolSencillo2 = Bin (Bin Nil "s" Nil) "b" (Bin Nil "c" Nil)