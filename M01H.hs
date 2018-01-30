import Data.List
-- exercicio 1
primo :: Int -> Bool
primo x | (x<=2) = True
        | otherwise = primo2 x (x-1)
primo2 :: Int -> Int -> Bool
primo2 x y | (y == 1) = True
           | (mod x y == 0) = False
           | otherwise = primo2 x (y-1)
-- exercicio 2
nprimo :: Int -> [Int]
nprimo n | (n==0) = []
         | otherwise = nprimo2 n 2 []
nprimo2 :: Int -> Int -> [Int] -> [Int]
nprimo2 n x v | (n==0) = sort v
              | (primo x == True) = nprimo2 (n-1) (x+1) (x:v)
              | otherwise = nprimo2 n (x+1) v
-- exercicio 3
goldbach :: Int -> [Int]
goldbach x = goldbach2 x (head(filter(>(div x 2)) (nprimo x)))

goldbach2 :: Int -> Int -> [Int]
goldbach2 x y = ((x-y):y:[])
