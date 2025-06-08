--Nos piden definir curry y uncurry. (Ya la hice en las guias)
--Algunos puntos importantes es tener en cuenta la asociatividad del operador de tipo ->
prod' :: Num a => (a, a) -> a
prod' (x, y) = x * y

prod :: Num a => a -> a -> a
prod x y = x * y

porcuatro :: Num a => a -> a
porcuatro = \x -> uncurry prod (4, x)

porcuatro' :: Num a => a -> a --Rascarse la oreja izq. con la mano der. (Súper non-sense)
porcuatro' = curry prod' 4

porcuatro'' :: Num a => a -> a
porcuatro'' = (*4)

--Tenemos en cuenta doble x
doble :: Num a => a -> a --Porque prod toma 'dos enteros', le estamos pasando uno, falta un entero.
doble = prod 2 --Qué tipo tiene?

sumarUno :: Int -> Int
sumarUno = (+) 1 --El operador más está de forma prefija.

--Definir las siguientes funciones de forma similar a (+) 1
--a)
triple :: Float -> Float
triple = (*) 3

--b)
esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (<=) 18

--Implementar y dar el tipo de las siguientes funciones
--a)
composicion :: (b -> c) -> (a -> b) -> a -> c
composicion f g x = f(g x) 
--b)
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y
--Tenemos un ejemplo aplicado donde nos sirve dar vuelta los parámetros.
--Recordemos que >= toma a >= b, al hacer flip', toma b >= a.
--No es una buena aplicación, queda muy confuso.
esMayorDeEdad' :: Int -> Bool
esMayorDeEdad' = flip' (>=) 18
--c)
--Ya está definida en el prelude. Se nota $, sirve para usar menos paréntesis.
aplicar :: (a -> b) -> a -> b
aplicar f = f
--Podemos poner aplicar = id
--Por ejemplo: (+) 4 $ (+) 1 3 tipa, pero sin el $ hace falta poner paréntesis.
--d) Esto en Haskell se llama cons
ignorar :: a -> b -> a
ignorar x _ = x
--Definición con lambda.
ignorar' :: a -> b -> a
ignorar' x = \y -> x

--Que hace flip ($) 0? Espera una función y a esta le aplica cero. Por ejemplo:

ordAlOrigen :: (Int -> Int) -> Int
ordAlOrigen = flip ($) 0

--Que hace esta función?
funcionSinNombre :: Int -> Bool
funcionSinNombre = (==0).(flip mod 2)
--Da true si el número es par.

--Listas
--Extensión: [1,2,3]: dar los elementos de forma explícita
--Secuencias: [3..7] = [3,4,5,6,7]
--Por comprensión:
listaRara :: [(Int, Int)]
listaRara = [(x, y) | x <- [0..5], y <- [0..3], x+y==4]
--Esta lista está armada a partir de dos listas, [0,1,2,3,4,5] y [0,1,2,3] y toma los pares (x, y)
--Tales que la primera componente pertenezca a la primera lista y la segunda a la segunda y sumen 4
--Esta es una muy buena forma de definir esPrimo, no tengo que calcular los divisores.
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = all (\d -> mod n d /= 0) [2..floor(sqrt (fromIntegral n))] 

primos :: [Int]
primos = [n | n <- [2..], esPrimo n]

--Evaluación lazy
infinitosUnos :: [Int]
infinitosUnos = 1 : infinitosUnos

nUnos :: Int -> [Int]
nUnos n = take n infinitosUnos
--Acá vemos que podemos elegir los nUnos de una lista de infinitos unos sin que se cuelgue.
--Esto porque Haskell tiene evaluación lazy. O sea, no calcula cosas de más, solo lo necesario para evaluar y computar lo que requiere.

--Funciones de alto orden (reciben y/o devuelven una función)
--Definamos estas funciones, con la precondición de que las listas tengan algún elemento.
maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) 
    | x >= maximo xs = x
    | otherwise = maximo xs

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) 
    | x <= minimo xs = x
    | otherwise = minimo xs

listaMasCorta :: [[a]] -> [a]
listaMasCorta [xs] = xs
listaMasCorta (xs:xss)
    | length xs < length (listaMasCorta xss) = xs
    | otherwise = listaMasCorta xss

--Siempre repetimos la sintaxis, cambiamos muy poco. Hagamos una generalización
--Hecho en guia1.hs con foldr1
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun f (x:xs)
    | f x (mejorSegun f xs) = x
    | otherwise = mejorSegun f xs

maximoMejor :: Ord a => [a] -> a
maximoMejor = mejorSegun (>=)

listaMasCortaMejor :: [[a]] -> [a]
listaMasCortaMejor = mejorSegun (\xs ys -> length xs < length ys)

--Filtrar elementos de una lista (filter)
deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter (\x -> length x == n)

soloPuntosFijosEnN :: Int -> [Int -> Int] -> [Int -> Int]
soloPuntosFijosEnN n = filter(\f -> f n == n)

--Map
--a)
reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado xs = reverse(map reverse xs) 

paresCuadrados :: [Int] -> [Int]
paresCuadrados = map(\x -> if even x then x*x else x)

--Reescribamos esto con filter y map.
listaComp :: (a -> Bool) -> (a -> b) -> [a] -> [b]
listaComp p f xs = [f x | x <- xs, p x]

listaComp' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
listaComp' p f xs = map f (filter p xs)