module Ex2  where
import Prelude

-- 2. Escreva uma função que devolva a diferença entre a soma de todos os números de Fibonacci
-- ímpares menores que 100.000 e a soma de todos os números de Fibonacci pares também
-- menores que 100.000.

ex2 :: Int -> Int
ex2 n = fiboAux (fibo n [0, 1])

-- essa função só existe pra não ter que processar fibonacci tudo denovo
fiboAux :: [Int] -> Int
fiboAux list = sum (filter odd list) - sum (filter even list)

last2:: [a] -> a
last2 list = head(tail(reverse list))

nextFibo:: [Int] -> Int
nextFibo list = last list + last2 list

fibo :: Int -> [Int] -> [Int]
fibo target list
    | nextFibo list >= target = list
    | otherwise = fibo target (list ++ [nextFibo list])