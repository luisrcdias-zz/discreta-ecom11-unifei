quickSort :: [Int] -> [Int]

quickSort [] = []
quickSort (cabeca:cauda) = quickSort [y | y <- cauda, y < cabeca] ++ [cabeca] ++ quickSort [y | y<-cauda, y>= cabeca]
