funcl :: Int-> Int

funcl x | x==0 = 1
        | x>2 = funcl(x-1)+funcl(x-2)
        | otherwise =3
