module AB (AB, vacio, insertarABB) where --Estoy exportando solo las funciones y el tipo de dato
--Pero no los constructores. Lo que impide crear árboles binarios arbitrarios.
--Una función dadas las funciones que exporté, es que solo se creen ABB.
data AB a = Nil | Bin (AB a) a (AB a)

vacio :: AB a
vacio = Nil

insertarABB :: Ord a => a -> AB a -> AB a
insertarABB x Nil = Bin Nil x Nil
insertarABB x (Bin i r d) = if x < r then Bin (insertarABB x i) r d else Bin i r (insertarABB x d)
--Es recursión primitiva porque en el then y en else no hago recursión en ambas ramas a la vez
--pero no en la dos, por lo que uso el arbol derecho en el then y el izquerod en el else sin recursión.

--Vuelvo al archivo original