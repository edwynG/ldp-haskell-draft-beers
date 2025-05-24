module Main where

import Utils -- Importamos el modulo Utils externamente como lo declaramos

-- | Determinar la cantidad de cerveza óptima que debe agregarse en los barriles para poder llenar n vasos desde un barril
findBestSolution :: Int -> (Barrel, Barrel, Barrel) -> (Int, (Barrel, Barrel, Barrel))
findBestSolution n (a, b, c)
    | n <= 0 || not (isSatisfied a b c n) = (0, (a, b, c))
    | otherwise =
        let -- Camino agregando a 'a'
            (addedFromA, stateA) = fromAToC (a, b, c) n 0

            -- Camino agregando a 'c'
            (addedFromC, stateC) = fromCToA (a, b, c) n 0

            -- Lista de soluciones válidas
            soluciones = filter (\(ag, est) -> iSolution est n)
                          [(addedFromA, stateA), (addedFromC, stateC)]
        in case soluciones of
            [] -> (0, (a, b, c))
            xs -> minimum xs -- Elige la de menor cantidad agregada

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