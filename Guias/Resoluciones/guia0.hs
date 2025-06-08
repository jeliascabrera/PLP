{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use elem" #-}
instance Show a => Show (AB a) where
    show Nil = "Nil"
    show (Bin left value right) =
        "Bin (" ++ show left ++ ") " ++ show value ++ " (" ++ show right ++ ")"
--2. Definir las siguientes funciones.
--a)

valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0 = x
                | otherwise = -x

--b)
bisiesto :: Int -> Bool
bisiesto year | mod year 4 /= 0 = False
              | mod year 100 == 0 && mod year 400 /= 0 = False
              | otherwise = True
--c)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
--d)
listaDeDivisoresPositivos :: Int -> [Int]
listaDeDivisoresPositivos n = [x | x <- [1..n], mod n x == 0]

esPrimo :: Int -> Bool
esPrimo n = length (listaDeDivisoresPositivos n) == 2

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length listaDePrimos
            where listaDePrimos = filter esPrimo (listaDeDivisoresPositivos n)

--3. Definir.
--a)
inverso :: Float -> Maybe Float
inverso x | x == 0 = Nothing
          | otherwise = Just (1/x)
--b)
aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right True) = 1
aEntero (Right False) = 0

--4. Definir las siguientes funciones sobre listas
limpiar :: String -> String -> String
limpiar _ [] = []
limpiar [] str = str
limpiar (x:xs) str = limpiar xs (filter (/= x) str)

difPromedio :: [Float] -> [Float]
difPromedio xs  = map ((-promedio) +) xs
        where promedio = sum xs / fromIntegral (length xs)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) = not(any (/= x) xs)

--5. Dado el siguiente modelo de árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)
--definir:
--a)
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

--b)
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

--c)
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = r * productoAB i * productoAB d
