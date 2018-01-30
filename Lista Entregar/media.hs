media :: [Float] -> Float
media [] = 0
media (x:y) = mediaux (x:y) 0 1

mediaux :: [Float] -> Float -> Float -> Float
mediaux (x:y) sum dive | (y==[]) = (sum+x)/(dive)
                       | otherwise = mediaux y (sum+x) (dive+1)
