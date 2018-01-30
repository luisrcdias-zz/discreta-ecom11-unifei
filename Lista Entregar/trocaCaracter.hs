trocaCaracter :: Char -> Char -> [Char] -> [Char]

trocaCaracter a b [] = []
trocaCaracter a b (x:xs) | (x == a) = b : trocaCaracter a b xs
                         |otherwise = x : trocaCaracter a b xs
