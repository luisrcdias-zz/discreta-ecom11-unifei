fatorial :: Int-> Int
fatoaux :: Int-> Int -> Int


fatorial x = fatoaux x 1

fatoaux x y  |(x>1) = fatoaux (x-1) (y*x)
             |(x==0) = 1
             |(x==1) = y
