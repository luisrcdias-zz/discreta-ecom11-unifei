nEsimo :: Int -> [Int] -> Int
finda :: Int -> [Int] -> Int -> Int
nEsimo a [] = 0
nEsimo a (x:y) = finda a (x:y) 1



finda a (x:y) aux | (a==aux) = x
                  | (a>aux) = 0
                  | otherwise = finda a y (aux+1)
