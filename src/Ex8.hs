module Ex8 where
import Ex2 ( fibo )

-- 8. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os
-- números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro
-- dado como argumento.

ex8 :: Int -> [Int]
ex8 n = fibo n [2, 1]