inverteLista ::[Int]->[Int]
inverteLista []=[]
inverteLista(x:y)= insere x (inverteLista y)

insere :: Int -> [Int] -> [Int]
insere e [] = [e]
insere e (x:y) = x:(insere e y)
