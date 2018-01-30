ePar :: Int -> Bool
ePar n = (mod n 2 ==0)

eImpar :: Int -> Bool
eImpar n = (mod n 2 == 1)

filtro :: (Int->Bool) -> [Int] -> [Int]
filtro p [] = []
filtro p (h:c) = if (p h) then h:filtro p c else filtro p c
