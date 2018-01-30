resto :: Int-> Int -> Int
restoaux :: Int-> Int -> Int -> Int


resto x y = restoaux x y 0

restoaux x y z |(x>y) = restoaux (x-1) y (z+1)
               |(x<y) = x
               |(z>=y) = restoaux z y 0
               |otherwise = z
