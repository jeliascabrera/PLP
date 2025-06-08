--Programación funcional parte 2
--maximoL
maximoL :: Ord a => [a] -> a
maximoL (x:xs) = foldr max x (x:xs) --Podemos usar foldr1

maximoL' :: Ord a => [a] -> a
maximoL' = foldr1 max

--Take 
take1 :: Int -> [a] -> [a] --No es recursión estructural porque cambia el parámetro n
take1 n [] = []
take1 n (x:xs) = if n == 0 then [] else x:take1 (n-1) xs

take2 :: [a] -> Int -> [a] --Ahora sí, porque mi función es (take2 xs), por lo que ya entra en la def. de recursión estructural. 
take2 [] n = []
take2 (x:xs) n = if n == 0 then [] else x:take2 xs (n-1)

take3 :: [a] -> (Int -> [a]) --Acá queda más explícito que recibe un parámetro y devuelve una función que espera otro parámetro.
                                --Es equivalente a la anterior.
take3 [] = const []
take3 (x:xs) = \n -> (if n == 0 then [] else x:take3 xs (n-1))

take' :: [a] -> Int -> [a]--La idea es que recibimos una lista y devolvemos una función que espera un entero y devuelve una lista
                            --Luego, puedo esperar ese nuevo elemento agregándolo al lambda o que el lambda devuelva otro.
take' = foldr (\x rec-> \n -> if n == 0 then [] else x:rec(n-1)) (const [])

--subListaQueMasSuma
--6. Esquema de recursión estructural
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)
sublistaQueMasSuma :: [Int] -> [Int]
sublistaQueMasSuma  = recr(\x xs rec ->
    if (sum.prefijoQueMasSuma) (x:xs) >= sum rec
        then prefijoQueMasSuma (x:xs)
        else rec
    ) []

prefijos :: [a] -> [[a]]--Aparte
prefijos xs = [take i xs | i <- [0..length xs]]--Aparte

mejorSegun :: (a -> a -> Bool) -> [a] -> a --Aparte
mejorSegun cond = foldr1 (\x y -> if cond x y then x else y) --Aparte

prefijoQueMasSuma :: [Int] -> [Int] --Aparte
prefijoQueMasSuma xs = mejorSegun (\ys zs -> sum(ys) > sum(zs)) (prefijos xs) --Aparte

--Generación infinita. Definamos
pares :: [(Int, Int)]
pares = [p | k <- [0..], p <- paresQueSuman k]--Genera efectivamente.

paresQueSuman :: Int -> [(Int, Int)]
paresQueSuman k = [(i, k-i) | i <- [0..k]]

paresMalHecho :: [(Int, Int)]
paresMalHecho = [(x, y) | x <- [0..], y <- [0..]] --Esto me fija la primera componente en cero.

--Sea el siguiente tipo
data AEB a = Hoja a | Bin (AEB a) a (AEB a) deriving (Show)
--Definamos el foldAEB

foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
foldAEB cHoja cBin arbol = case arbol of 
                        Hoja x -> cHoja x
                        (Bin i r d) -> cBin (rec i) r (rec d)
                    where rec = foldAEB cHoja cBin

ramas :: AEB a -> [[a]]
ramas = foldAEB (\x -> [[x]]) (\recI r recD -> map (r:) (recI++recD))
--Recordemos que map f xs ++ map f ys == map f (xs++ys)
espejo :: AEB a -> AEB a
espejo = foldAEB Hoja (\recI r recD -> Bin recD r recI)


--Usar esquemas de recursión evita problemas a la larga (olvidarse de casos o colgarse)
--Además de darle más legibilidad y compactar el código.
--Ejemplo de AEB
miArbol = Bin (Hoja 3) 5 (Bin (Hoja 7) 8 (Hoja 1))

data AB a = Nil | BinAB (AB a) a (AB a)

insertarABB :: Ord a => a -> AB a -> AB a
insertarABB x Nil = BinAB Nil x Nil
insertarABB x (BinAB i r d) = if x < r then BinAB (insertarABB x i) r d else BinAB i r (insertarABB x d)
--Es recursión primitiva porque en el then y en else no hago recursión en ambas ramas a la vez
--pero no en la dos, por lo que uso el arbol derecho en el then y el izquerod en el else sin recursión.

truncar :: AB a -> Int -> AB a
truncar Nil _ = Nil
truncar (BinAB i r d) n = if n == 0 then Nil else BinAB (truncar i (n-1)) r (truncar d (n-1))

--No está escrito como recursión estructural pero se puede. Igual que hicimos con el take3.
--Que pasa si quiero crear una estructura ABB, con el data, necesito alguna forma
--para no permitir usar los constructores de forma libre.
--Es decir, no permito que se creen todas las estructuras creables.
--Sigo en clase1abrilaux.hs


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

--RoseTree
data RoseTree a = Rose a [RoseTree a]
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose cRose (Rose x hijos) = cRose x (map rec hijos) --Este map nos da una [b]
    where rec = foldRose cRose

altura :: RoseTree a -> Int
altura = foldRose (\x alturaHijos -> if null alturaHijos then 1 else 1 + maximum alturaHijos)

ramasRoseTree :: RoseTree a -> [[a]]
ramasRoseTree = foldRose (\raiz subRamas -> if null subRamas 
    then [[raiz]]
    else concatMap (map (raiz :)) subRamas)


arbol = Rose 1 [Rose 2 []]

type Conj a = (a -> Bool) --Conjunto caracterizado por su función de pertenencia.

pertenece :: a -> Conj a -> Bool
pertenece x f = f x

--vacio
vacio :: Conj a
vacio = const False


agregar :: (Eq a) => a -> Conj a -> Conj a
agregar x f = (\y -> x == y || f y)



