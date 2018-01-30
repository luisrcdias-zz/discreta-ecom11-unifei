inverteLista ::[Char]->[Char]
inverteLista []=[]
inverteLista(x:y)= insere x (inverteLista y)

insere :: Char -> [Char] -> [Char]
insere e [] = [e]
insere e (x:y) = x:(insere e y)

palindromo :: [Char] -> Bool

palindromo (x:y) | (inverteLista (x:y) == (x:y)) = True
                 | otherwise = False
