mergeVector :: [Char] -> [Char] -> [Char]

mergeVector [] v = v
mergeVector v [] = v
mergeVector (x:y) (z:w) | (y==[]) = (x:z:w)
                        | otherwise = x:(mergeVector y (z:w))
