replace :: Int -> Int -> [Int] -> [Int]

replace a b (x:y) = replaceaux a b (x:y) [0]

replaceaux :: Int -> Int -> [Int] -> [Int] -> [Int]

replaceaux a b (x:y) (k:z) | (y==[]) = (k:z)[]
                           | ([x]==[]) = (k:z)
                           | (b==x) = ((z)++(a:replaceaux a b y (k:z)))
                           | otherwise = x:replaceaux a b y (k:z)
