comp :: [Int] -> Int



comp[] = comp[]
comp(cabeca:cauda) = cabeca + sum cauda



ordena ::[Int]->[Int]
ordena []=[]
ordena(x:y)= insere x (ordena y)

insere :: Int -> [Int] -> [Int]
insere e [] = [e]
insere e (x:y) |(e<=x) = e:x:y
               |otherwise = x:insere e y
