resto :: Int-> Int -> Int
quociente :: Int -> Int -> Int
fatorial :: Int -> Int
resto x y |x>y = resto (x-y) y
          |x<y = x
          | otherwise =0

quociente x y |x<y = 0
              |otherwise = 1 + quociente (x-y) y

fatorial a | (a<=1) = 1
           | otherwise = a*fatorial (a-1)
