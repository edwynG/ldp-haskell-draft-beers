type Barrel = (Int, Int)

-- | Validamos que los barriles tengan capacidad > 0 y cantidad >= 0
validateBarrel :: Barrel -> Bool
validateBarrel (cap, curr) = cap > 0 && curr >= 0

-- | Creamos una terna de barriles validados, preservando cantidades si son válidas
initialBarrels :: Barrel -> Barrel -> Barrel -> (Barrel, Barrel, Barrel)
initialBarrels a b c = (validate a, validate b, validate c)
  where
    validate (cap, curr)
      | validateBarrel (cap, curr) = (cap, curr)
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