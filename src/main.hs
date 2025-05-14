module Main where

import Utils (helloWorld) -- Importamos el modulo Utils externamente como lo declaramos

main :: IO ()
main = putStrLn (helloWorld)