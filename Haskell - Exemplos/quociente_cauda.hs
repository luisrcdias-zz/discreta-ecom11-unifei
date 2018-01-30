quociente :: Int -> Int -> Int
quocaux :: Int -> Int -> Int -> Int
quociente x y = quocaux x y 1

quocaux x y z |(x==y) = z
              |(x<y) = (z-1)
              |otherwise = quocaux (x-y) y (z+1)
