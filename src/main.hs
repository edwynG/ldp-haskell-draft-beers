module Main where

import Utils (Barrel, validateBarrel, initialBarrels) -- Importamos el modulo Utils externamente como lo declaramos

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

main :: IO ()
main = putStrLn ("Hello world!!")