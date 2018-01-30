type ConjuntoInt = [Int]
type RelacaoInt = [(Int,Int)]

reflexiva :: ConjuntoInt->RelacaoInt->Bool
reflexiva [] _ = True
reflexiva (a:b) r
    | elem (a,a) r == True = reflexiva b r
    | otherwise = False

irreflexiva :: ConjuntoInt-> RelacaoInt-> Bool
irreflexiva [] _ = True
irreflexiva (a:b) r
  | (elem (a,a) r) = False
  | otherwise = irreflexiva b r

simetrica :: ConjuntoInt->RelacaoInt-> Bool
simetrica _ r = simetricaaux r r

  where

    simetricaaux :: RelacaoInt -> RelacaoInt -> Bool
    simetricaaux [] _ = True
    simetricaaux ((x, y):b) r | (not(elem (y, x) r)) = False
                              | otherwise = simetricaaux b r

antissimetrica :: ConjuntoInt->RelacaoInt-> Bool
antissimetrica _ r = antissimetricaaux r r

  where

    antissimetricaaux :: RelacaoInt -> RelacaoInt -> Bool
    antissimetricaaux [] _ = True
    antissimetricaaux ((x, y):b) r
      | elem (y, x) r && y /= x = False
      | otherwise = antissimetricaaux b r

asimetrica :: ConjuntoInt->RelacaoInt-> Bool
asimetrica _ r = aux r r

  where

    aux :: RelacaoInt -> RelacaoInt -> Bool
    aux [] _ = True
    aux ((x, y):b) r
      | elem (y, x) r = False
      | otherwise = aux b r

transitiva :: ConjuntoInt->RelacaoInt->Bool
transitiva _ r = transitivaAux r r
  where
    transitivaAux :: RelacaoInt->RelacaoInt->Bool
    transitivaAux [] _ = True
    transitivaAux ((x,y):r1) r = (testaPares x (geraPares y r) r) && transitivaAux r1 r

    -- Gera uma lista com os elementos com os quais y se relaciona
    geraPares :: Int -> RelacaoInt-> ConjuntoInt
    geraPares _ [] = []
    geraPares y ((a,x):r1)
      | a == y = x:geraPares y r1
      | otherwise = geraPares y r1

    -- Verifica se x se relaciona com todos os elementos com os quais y se relaciona
    testaPares :: Int -> ConjuntoInt->RelacaoInt->Bool
    testaPares _ [] _ = True
    testaPares x (a:b) r
      | not (elem (x,a) r) = False
      | otherwise = testaPares x b r


--    Essa Funcao testa se há algum par repetido dentro do conjunto de relação otherwise
-- testa1 testa se um par existe em R, caso exista ela chama a função testa2
-- que testa se o par tambem existe no restante da lista.

funcional :: ConjuntoInt->ConjuntoInt->RelacaoInt->Bool
funcional [] _ _ = True
funcional (a:b) c r
  | testa1 a c r = False
  | otherwise = funcional b c r

  where
    testa1 :: Int->ConjuntoInt->RelacaoInt->Bool
    testa1 _ [] _ = False
    testa1 a (b:c) r
      | elem (a,b) r = testa2 a c r
      | otherwise = testa1 a c r

    testa2 :: Int->ConjuntoInt->RelacaoInt->Bool
    testa2 _ [] _ = False
    testa2 a (b:c) r
      | elem (a,b) r = True
      | otherwise = testa2 a c r

-- Mesmo processo da funcional
injetora :: ConjuntoInt->ConjuntoInt->RelacaoInt->Bool
injetora _ [] _ = True
injetora a (b:t) r
  | testa1 a b r = False
  | otherwise = injetora a t r

  where
    testa1 :: ConjuntoInt->Int->RelacaoInt->Bool
    testa1 [] _ _ = False
    testa1 (a:t) b r
      | elem (a,b) r = testa2
      | otherwise = testa1 t b r
    testa2 :: ConjuntoInt->Int->RelacaoInt->Bool
    testa2 [] _ _ = False
    testa2 (a:t) b r
      | elem (a,b) r = True
      | otherwise = testa2 t b r

-- Simplesmente testa se todos os elementos de a estao no conjunto da
-- relacao. Caso nao esteja retorna False, caso contrario True.
total :: ConjuntoInt->ConjuntoInt->RelacaoInt->Bool
total [] _ _ = True
total (a:t) b r
  | aux a b r = total t b r
  | otherwise = False

  where
    --- Verifica se (a, _) pertence a r
    aux :: Int -> ConjuntoInt -> RelacaoInt -> Bool
    aux _ [] _ = False
    aux a (b:t) r
      | elem (a,b) r = True
      | otherwise = aux a t r

sobrejetora :: ConjuntoInt->ConjuntoInt->RelacaoInt->Bool
sobrejetora _ [] _ = True
sobrejetora a (b:t) r
  | aux a b r = sobrejetora a t r
  | otherwise = False

  where
    aux :: ConjuntoInt->Int->RelacaoInt->Bool
    aux [] _ _ = True
    aux (a:t) b r
      | elem(a, b) = True
      | otherwise = aux t b r

ordemParcialEstrita :: ConjuntoInt->RelacaoInt-> Bool
ordemParcialEstrita a r
  | irreflexiva a r && antissimetrica a r && transitiva a r = True
  | otherwise = False
