insereUltimo :: Int -> [Int] -> [Int]
insereUltimo a [] = [a]
insereUltimo a (x:y) | (y==[]) = x:[a]
                     | otherwise = x:insereUltimo a y
