soma :: [Int] -> Int

soma [] = 0
soma (x:y) = x+soma y
