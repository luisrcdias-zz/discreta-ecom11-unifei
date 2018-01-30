remove :: Int -> [Int] -> [Int]

remove a (x:y) = replaceaux a (x:y) [0]

replaceaux :: Int -> [Int] -> [Int] -> [Int]

replaceaux a (x:y) (k:z) | (y==[]) = ([k])
                         | (a==x) = ((z)++(replaceaux a y (k)))
                         | otherwise = x:replaceaux a y (k:z)
