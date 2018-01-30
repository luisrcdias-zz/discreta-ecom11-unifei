insereN :: Int -> Int -> [Int] -> [Int]

insereN a b (x:y) = insereaux a b 0 (x:y) [0]

insereaux :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]

insereaux a b aux (x:y) (k:z) | (b==aux) = ((z)++(a:x:y))
                              | otherwise = x:insereaux a b (aux+1) y (k:z)
