module Utils (Barrel, validateBarrel, initialBarrels) where -- Los modulos tiene que estar con la primera letra en mayuscula

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