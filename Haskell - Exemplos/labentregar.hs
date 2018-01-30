-- Trabalho 2

-- 1

nEsimo :: Int -> [Int] -> Int

nEsimo n (x:v) | (n==1) = x

               | otherwise = nEsimo (n-1) v



-- 2

inserePrimeiro :: Int -> [Int] -> [Int]

inserePrimeiro x v = x:v



-- 3

insereUltimo :: Int -> [Int] -> [Int]

insereUltimo a [] = [a]

insereUltimo a (x:y) | (y==[]) = x:[a]

                     | otherwise = x:insereUltimo a y





-- 4

inserePosicao :: Int -> [Int] -> Int -> [Int]

inserePosicao x (y:v) n | (n==1) = x:y:v

                        | otherwise = y:inserePosicao x v (n-1)



-- 5

retiraCaracter :: Char -> [Char] -> [Char]

retiraCaracter x (y:v) | (v==[] && x==y) = v

                       | (v==[]) = y:v

                       | (x==y) = retiraCaracter x v

                       | otherwise = y:retiraCaracter x v



-- 6

trocaCaracter :: Char -> Char -> [Char] -> [Char]

trocaCaracter x z (y:v) | (v==[] && x==y) = z:v

                        | (v==[]) = y:v

                        | (x==y) = z:trocaCaracter x z v

                        | otherwise = y:trocaCaracter x z v



-- 7

inverteLista ::[Int]->[Int]

inverteLista []=[]

inverteLista(x:y)= insere x (inverteLista y)



insere :: Int -> [Int] -> [Int]

insere e [] = [e]

insere e (x:y) = x:(insere e y)





-- 8

serieFibonacci :: Int -> [Int]

serieFibonacci n = serieFibonacciAux n 0 1



serieFibonacciAux :: Int -> Int -> Int -> [Int]

serieFibonacciAux n x y | (n==0) = []

                        | otherwise = x:serieFibonacciAux (n-1) y (x+y)



-- 9

inverteListaChar ::[Char]->[Char]

inverteListaChar []=[]

inverteListaChar(x:y)= insereChar x (inverteListaChar y)



insereChar :: Char -> [Char] -> [Char]

insereChar e [] = [e]

insereChar e (x:y) = x:(insereChar e y)



palindromo :: [Char] -> Bool



palindromo (x:y) | (inverteListaChar (x:y) == (x:y)) = True

                 | otherwise = False





-- 10

substring :: [Char] ->  Int -> Int-> [Char]

substring (x:v) y z | (y==z) = x:[]

                    | (y/=0) = substring v (y-1) (z-1)

                    | otherwise = x:substring v y (z-1)



-- 11

mergeVector :: [Char] -> [Char] -> [Char]



mergeVector [] v = v

mergeVector v [] = v

mergeVector (x:y) (z:w) | (y==[]) = (x:z:w)

                        | otherwise = x:(mergeVector y (z:w))



insereString :: [Char] -> Int -> [Char] -> [Char]

insereString x n (y:v) | (n==1) = mergeVector x (y:v)

                       | otherwise = y:insereString x (n-1) v



-- 12

subString :: [Char] -> [Char] -> Bool

subString x (y:v) | (v==[]) = False

                  | (stringEquals x (y:v)) = True

                  | otherwise = subString x v



stringEquals :: [Char] -> [Char] -> Bool

stringEquals (x:v) (y:b) | (v==[] && x==y) = True

                         | (b==[]) = False

                         | (x==y) = stringEquals v b

                         | otherwise = False



-- 13

media :: [Float] -> Float

media v = mediaAux 1 0 v



mediaAux :: Float -> Float -> [Float] -> Float

mediaAux x soma (y:v) | (v==[]) = (soma+y)/x

                      | otherwise = mediaAux (x+1) (soma+y) v



-- 14

mediana :: [Float] -> Float

mediana v | (mod (tamanhoVetor v 0) 2 == 0) = medianaAux v (div (tamanhoVetor v 0) 2) False

          | otherwise = medianaAux v (div (tamanhoVetor v 0) 2) True



medianaAux :: [Float] -> Int -> Bool -> Float

medianaAux (x:v) n b | (n==0 && b==True) = x

                     | (n==1 && b==False) = (x + medianaAux v (n-1) True)/2

                     | otherwise = medianaAux v (n-1) b



tamanhoVetor :: [Float] -> Int -> Int

tamanhoVetor (x:v) n | (v==[]) = n+1

                     | otherwise = tamanhoVetor v (n+1)



-- 15

moda :: [Float] -> Float

moda v = modaAux v v 0 []



modaAux :: [Float] -> [Float] -> Float -> [Float] -> Float

modaAux (x:v) memoria vezes vetor | (v==[] && (aparece memoria x 0)>vezes) = x

                                  | (v==[]) = head(vetor)

                                  | ((aparece memoria x 0)>vezes) = modaAux v memoria (aparece memoria x 0) (x:vetor)

                                  | otherwise = modaAux v memoria vezes vetor



aparece :: [Float] -> Float -> Float -> Float

aparece (x:v) num vezes | (v==[] && num==x) = vezes+1

                        | (v==[]) = vezes

                        | (num==x) = aparece v num (vezes+1)

                        | otherwise = aparece v num vezes

