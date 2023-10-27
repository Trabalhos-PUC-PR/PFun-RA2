module Ex5 where
import Data.List ((\\))
import Prelude

-- 5. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função
-- que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores
-- que um determinado inteiro dado.
-- pesquisa: https://programmingpraxis.com/2011/02/25/sieve-of-euler/

ex5 :: Int -> [Int]
ex5 n = reverse (ex5Aux (2:[3,5..n]) [])

ex5Aux :: [Int] -> [Int] -> [Int]
ex5Aux [] primesList = primesList 
ex5Aux inputList primes = ex5Aux (tail (ex5diff inputList)) primes++[head(ex5diff inputList)]

ex5diff :: [Int] -> [Int]
ex5diff input = (\\) input (map (* head input) input)


-- -- First, make a list of numbers from 2, as large as you wish; call the maximum number n.

-- -- Second, extract the first number from the list, make a new list in which each element of 
--   the original list, including the first, is multiplied by the extracted first number.

-- -- Third, “subtract” the new list from the original, keeping in an output list only those 
-- numbers in the original list that do not appear in the new list.

-- -- Fourth, output the first number from the list, which is prime, and repeat the second, 
-- third and fourth steps on the reduced list excluding its first element, continuing until the input list is exhausted.





--   print (ex3 100_000_005)