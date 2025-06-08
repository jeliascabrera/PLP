--Clase teórica sobre esquemas de recursión y tipos de datos inductivos, definamos
sumaL :: [Int] -> Int
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs:xss) = xs ++ concatena xss

reverso :: [a] -> [a]
reverso (x:xs) = reverso xs ++ [x]
--Usando foldr y recr
reversoFoldr :: [a] -> [a]
reversoFoldr = foldr (\x rec -> rec ++ [x]) []

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

trimRecr :: String -> String
trimRecr = recr (\x xs rec -> if x == ' ' then rec else x:xs) []

--Relaciones entre recursiones
--Definir foldr en términos de recr
