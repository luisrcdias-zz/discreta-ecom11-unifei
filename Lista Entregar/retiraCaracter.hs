retiraCaracter :: Char -> [Char] -> [Char]
retiraCaracter a [] = []
retiraCaracter a (x:y)|(x==a) = retiraCaracter a y
                      |otherwise = x:retiraCaracter a y
