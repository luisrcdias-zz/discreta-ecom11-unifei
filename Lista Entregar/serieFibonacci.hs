fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



serieFibonacci :: Int -> [Int]
serieFibonacci 1 = [fib 1]
serieFibonacci a = fib a:(serieFibonacci (a-1))
