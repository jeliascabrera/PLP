--Razonamiento ecuacional
prod :: Either Int (Int, Int ) -> Either Int (Int, Int ) -> Either Int (Int, Int )
prod (Left x) (Left y) = Left (x * y)
prod (Left x) (Right (y, z)) = Right (x * y, x * z)
prod (Right (y, z)) (Left x) = Right (y * x, z * x)
prod (Right (y, z)) (Right (x, w)) = Left (y * x + z * w)

--Queremos demostrar que para todo p, q de tipo Either Int (Int, Int),
--prod p q = prod q p
