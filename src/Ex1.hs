module Ex1 where
import Prelude

-- 1. Escreva uma função que devolva a soma de todos os números menores que 10.000 que sejam
-- múltiplos de 3 ou 5 e não sejam múltiplos de 2.

ex1 = sum (ex1Logic 0 [])
ex1Logic :: Int -> [Int] -> [Int]
ex1Logic 10000 acc = acc
ex1Logic current acc
    | odd current && (mod current 5 == 0 || mod current 3 == 0) = ex1Logic (current+1) (acc++[current])   
    | otherwise = ex1Logic (current+1) acc