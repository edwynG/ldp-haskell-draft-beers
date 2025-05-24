module Utils where -- Los modulos tiene que estar con la primera letra en mayuscula

type Barrel = (Int, Int)

isSatisfied:: Barrel -> Barrel -> Barrel -> Int -> Bool
isSatisfied a b c n =  fst a >= n || fst b >= n || fst c >= n

-- | Validamos que los barriles tengan capacidad > 0 y cantidad >= 0
validateBarrel :: Barrel -> Bool
validateBarrel (cap, curr) = cap > 0 && curr >= 0 && curr <= cap

-- | Creamos una terna de barriles validados, preservando cantidades si son válidas
initialBarrels :: Barrel -> Barrel -> Barrel -> (Barrel, Barrel, Barrel)
initialBarrels a b c = (validate a, validate b, validate c)
  where
    validate (cap, curr)
      | validateBarrel (cap, curr) = (cap, curr)
      | cap < 0                    = (0,0)
      | curr > cap                 = (cap, cap)
      | curr < 0                   = (cap,0)                
      | otherwise                  = (cap, 0)

-- | Verificamos si es posible servir exactamente n vasos de cerveza desde un barril
iSolution :: (Barrel, Barrel, Barrel) -> Int -> Bool
iSolution (barrelA, barrelB, barrelC) n
    | n < 0     = False
    | not (validateBarrel barrelA) || not (validateBarrel barrelB) || not (validateBarrel barrelC) = False
    | otherwise = 
        let (capA, currA) = barrelA
            (capB, currB) = barrelB
            (capC, currC) = barrelC
        in (capA >= n && currA >= n) || (capB >= n && currB >= n) || (capC >= n && currC >= n)

-- | agregamos cerveza a un barril, luego retornamos el barril y el desbordamiento que hubo
addBeer :: Int -> Barrel -> (Barrel, Int)
addBeer n (cap, curr)
    | n <= 0 = ((cap, curr), 0)
    | otherwise = let newCurr = min (curr + n) cap
                  in ((cap, newCurr), n - (newCurr - curr))

-- | Agrega de A a C de uno en uno
fromAToC :: (Barrel, Barrel, Barrel) -> Int -> Int -> (Int, (Barrel, Barrel, Barrel))
fromAToC (a, b, c) n contador
    | iSolution (a, b, c) n = (contador, (a, b, c))
    | otherwise =
        let (nuevoA, desbordeA) = addBeer 1 a
            (nuevoB, desbordeB) = addBeer desbordeA b
            (nuevoC, _)         = addBeer desbordeB c
        in fromAToC (nuevoA, nuevoB, nuevoC) n (contador + 1)

-- | Agrega de C a A de uno en uno
fromCToA :: (Barrel, Barrel, Barrel) -> Int -> Int -> (Int, (Barrel, Barrel, Barrel))
fromCToA (a, b, c) n contador
    | iSolution (a, b, c) n = (contador, (a, b, c))
    | otherwise =
        let (nuevoC, desbordeC) = addBeer 1 c
            (nuevoB, desbordeB) = addBeer desbordeC b
            (nuevoA, _)         = addBeer desbordeB a
        in fromCToA (nuevoA, nuevoB, nuevoC) n (contador + 1)

