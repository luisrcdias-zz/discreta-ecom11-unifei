type Ponto = (Float,Float)

distancia :: Ponto->Ponto->Float

distancia (x1,y1) (x2,y2) =
  sqrt ((sqrtdif x1 x2)+sqrtdif y1 y2)
 where sqrtdif a b = (a-b)^2
