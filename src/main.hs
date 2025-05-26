module Main where

import Utils -- Importamos el modulo Utils externamente como lo declaramos

-- Desborda desde A hacia B, luego el desborde de B se reparte
overflowFromA :: (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
overflowFromA (a, b, c) =
    let (a', overflowA) = if snd a > fst a then ( (fst a, fst a), snd a - fst a ) else (a, 0)
        (b', overflowB) = if snd b + overflowA > fst b
                          then ( (fst b, fst b), snd b + overflowA - fst b )
                          else ( (fst b, snd b + overflowA), 0 )
    in overflowFromB (a', b', c) overflowB

-- El desborde de B va al barril vecino con menor cantidad actual
overflowFromC :: (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
overflowFromC (a, b, c) =
    let (c', overflowC) = if snd c > fst c then ( (fst c, fst c), snd c - fst c ) else (c, 0)
        (b', overflowB) = if snd b + overflowC > fst b
                          then ( (fst b, fst b), snd b + overflowC - fst b )
                          else ( (fst b, snd b + overflowC), 0 )
    in overflowFromB (a, b', c') overflowB

-- El desborde de B va al barril vecino con menor cantidad actual
overflowFromB :: (Barrel, Barrel, Barrel) -> Int -> (Barrel, Barrel, Barrel)
overflowFromB (a, b, c) overflowB
    | overflowB <= 0 = (a, b, c)
    | snd a <= snd c && snd a < fst a =
        let spaceA = fst a - snd a
            transferA = min overflowB spaceA
            a' = (fst a, snd a + transferA)
            lost = overflowB - transferA
        in (a', b, c) -- Si quieres llevar la cuenta de lo perdido, puedes devolverlo también
    | snd c < fst c =
        let spaceC = fst c - snd c
            transferC = min overflowB spaceC
            c' = (fst c, snd c + transferC)
            lost = overflowB - transferC
        in (a, b, c')
    | otherwise = (a, b, c) -- Todo el desborde se pierde

-- | Determinar la cantidad de cerveza óptima que debe agregarse en los barriles para poder llenar n vasos desde un barril
findBestSolution :: Int -> (Barrel, Barrel, Barrel) -> (Int, (Barrel, Barrel, Barrel))
findBestSolution n (a, b, c)
    | n < 0 || not (isSatisfied a b c n) = (0, (a, b, c))
    | otherwise =
        let 
            (a', b', c') = overflowFromA (a, b, c)
            (a'', b'', c'') = overflowFromC (a', b', c')

            -- Camino agregando a 'a'
            (addedFromA, stateA) = fromAToC (a'', b'', c'') n 0

            -- Camino agregando a 'c'
            (addedFromC, stateC) = fromCToA (a'', b'', c'') n 0

            -- Lista de soluciones válidas
            solutions = [(addedFromA, stateA), (addedFromC, stateC)]
        in 
            minimum solutions -- Elige la de menor cantidad agregada

main :: IO ()
main = do
    putStrLn "Barril A: "
    capA <- getLine
    putStrLn "contiene"
    currA <- getLine
    putStrLn "Barril B: "
    capB <- getLine
    putStrLn "contiene"
    currB <- getLine
    putStrLn "Barril C: "
    capC <- getLine
    putStrLn "contiene"
    currC <- getLine
    putStrLn "objetivo:"
    objetivo <- getLine

    let a = (read capA :: Int, read currA :: Int)
    let b = (read capB :: Int, read currB :: Int)
    let c = (read capC :: Int, read currC :: Int)
    let (barrilA, barrilB, barrilC) = initialBarrels a b c
    let n = read objetivo :: Int

    let (agregado, (a', b', c')) = findBestSolution n (barrilA, barrilB, barrilC)

    if  iSolution (a', b', c') n
        then
            if (fst a' >= n && snd a' >= n)
                then putStrLn $ "agregar " ++ show agregado ++ "L de cerveza desde el Barril A para servir los " ++ show n ++ " vasos de cerveza desde el"
            else if (fst c' >= n && snd c' >= n)
                then putStrLn $ "agregar " ++ show agregado ++ "L de cerveza desde el Barril C para servir los " ++ show n ++ " vasos de cerveza desde el"
            else if (snd a' >= fst a')
                then putStrLn $ "agregar " ++ show agregado ++ "L de cerveza desde el Barril A para servir los " ++ show n ++ " vasos de cerveza desde el Barril B"
            else
                putStrLn $ "agregar " ++ show agregado ++ "L de cerveza desde el Barril C para servir los " ++ show n ++ " vasos de cerveza desde el Barril B"
        else
            putStrLn "No es posible servir la cantidad deseada."