module Ex4 where

-- 4. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos
-- quadrados e o quadrado da soma destes inteiros.

ex4 :: [Int] -> Int
ex4 lista = sum(map (^ 2) lista) - (^) (sum lista) 2