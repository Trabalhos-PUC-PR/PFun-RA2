module Ex6 where
import Prelude

-- 6. Escreva uma função, usando iterate que devolva uma lista infinita de inteiros de tal forma que o
-- inteiro 𝑛 será o dobro do inteiro 𝑛 − 1. Esta função deve receber o valor inicial da lista.

ex6 :: Int -> [Int]
ex6 = iterate (2*) 