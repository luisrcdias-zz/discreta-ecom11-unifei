insereString :: [Char] -> Int -> [Char] -> [Char]

insereString (x:y) a (b:c) = insereStringAux (x:y) a 0 (b:c) []

insereStringAux :: [Char] -> Int -> Int -> [Char] -> [Char] -> [Char]
insereStringAux (x:y) a pos (b:c) (w:k) | ( a == pos ) = x:y
                                        | otherwise = "a"
