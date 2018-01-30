soma :: Int -> Int
soma x = somaux x 0

somaux :: Int -> Int -> Int
somaux x y | ( x == 0 ) = y
           | ( ( mod x 2) == 1 ) = somaux (x-1) (y+x)
           | otherwise = somaux (x-1) y
