module Main where
import Prelude
import GHC.Float.RealFracMethods (floorFloatInt)
import Data.List (sort, (\\), intersect)

-- Grupo:
-- André Luiz Kovalski
-- Gabrielle Louise
-- Leonardo Ikeda
-- Leonardo Knight

  -- Todas as questões apresentadas a seguir devem ser respondidas por meio de códigos em Haskell
  -- e disponibilizados em um ambiente onde seja possível ler e executar estes códigos, sem qualquer ação
  -- que inclua qualquer tipo de dowload, cópia ou adequação do código a outro ambiente que não o
  -- ambiente correspondente ao link enviado. O ambiente sugerido para uso é o ambiente disponível no
  -- site Repl.it.

  -- 1. Escreva uma função que devolva a soma de todos os números menores que 10.000 que sejam
  -- múltiplos de 3 ou 5 e não sejam múltiplos de 2.
ex1 = sum (ex1Logic 0 [])
ex1Logic :: Int -> [Int] -> [Int]
ex1Logic 10000 acc = acc
ex1Logic current acc
 | odd current && (mod current 5 == 0 || mod current 3 == 0) = ex1Logic (current+1) (acc++[current])   
 | otherwise = ex1Logic (current+1) acc

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

  -- 3. Fatorar um número em seus divisores primos é uma tarefa importante para a segurança de dados.
  -- Escreva uma função que devolva os fatores primos de qualquer inteiro dado maior que
  -- 100.000.000 e menor que 1.000.000.000.

-- Find the square root of the integer number n and round down to the closest whole number. Let's call this number s.
-- Start with the number 1 and find the corresponding factor pair: n ÷ 1 = n. So 1 and n 
    -- are a factor pair because division results in a whole number with zero remainder.
-- Do the same with the number 2 and proceed testing all integers (n ÷ 2, n ÷ 3, n ÷ 4... n ÷ s) up through the square 
    -- root rounded to s. Record the factor pairs where division results in whole integer numbers with zero remainders.
-- When you reach n ÷ s and you have recorded all factor pairs you have successfully factored the number n.
isPrime :: Int -> Int -> Bool
isPrime i 0 = False
isPrime i 1 = False
isPrime i num
  | i == num = True
  | mod num i == 0 = False
  | otherwise = isPrime (i+1) num 

getFactorPair:: Int -> Int -> [Int]
getFactorPair target y
 |  mod target y == 0 = [div target y, y]
 | otherwise = []

getRoundedDownSqrt :: Int -> Int
getRoundedDownSqrt target = floorFloatInt(sqrt (fromIntegral target))

-- getFactors
ex3 :: Int -> [Int]
ex3 target = filter (isPrime 2) (sort(reduce (getFactorPair target) [1..getRoundedDownSqrt target] []))

reduce :: (Int -> [Int]) -> [Int] -> [Int] -> [Int]
reduce _ [] acc = acc 
reduce function list acc = reduce function (init list) (function (last list)++acc)

  -- 4. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos
  -- quadrados e o quadrado da soma destes inteiros.
ex4 :: [Int] -> Int
ex4 lista = sum(map (^ 2) lista) - (^) (sum lista) 2

  -- 5. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função
  -- que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores
  -- que um determinado inteiro dado.
  -- https://programmingpraxis.com/2011/02/25/sieve-of-euler/
ex5 :: Int -> [Int]
ex5 n = reverse (ex5Aux [2..n] [])

ex5Aux :: [Int] -> [Int] -> [Int]
ex5Aux [] primesList = primesList 
ex5Aux inputList primes = ex5Aux (tail (ex5diff inputList)) primes++[head(ex5diff inputList)]

ex5diff :: [Int] -> [Int]
ex5diff input = (\\) input (map (* head input) input) 

  -- 6. Escreva uma função, usando iterate que devolva uma lista infinita de inteiros de tal forma que o
  -- inteiro 𝑛 será o dobro do inteiro 𝑛 − 1. Esta função deve receber o valor inicial da lista.

  -- 7. Escreva uma função que receba uma string e devolva outra string com as vogais trocadas. De tal
  -- forma que: 𝑎 será transformado em 𝑢; 𝑒 será transformado em 𝑜; 𝑖 não será transformado; 𝑜 será
  -- transformado em 𝑒 e 𝑢 será transformado em 𝑎;
ex7 :: String -> String
ex7 "" = ""
ex7 input
  | head input == 'a' = 'u':ex7 (tail input)
  | head input == 'e' = 'o':ex7 (tail input)
  | head input == 'o' = 'e':ex7 (tail input)
  | head input == 'u' = 'a':ex7 (tail input)
  | otherwise = head input :ex7 (tail input)

  -- 8. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os
  -- números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro
  -- dado como argumento.
ex8 :: Int -> [Int]
ex8 n = fibo n [2, 1]

  -- 9. Escreva uma função análoga a função map, disponível no prelude, na qual um predicado, função,
  -- seja aplicado a duas listas e não a apenas uma. Por exemplo 𝑚𝑎𝑝2 (+) [1,2,3] [10,11,12] deve
  -- retornar [11,13,15]
ex9 :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
ex9 _ [] _ = []
ex9 _ _ [] = []
ex9 func param1 param2 = func(head param1) (head param2) : ex9 func (tail param1) (tail param2) 

main::IO()
main = do
  print ex1
  print (ex2 100000)
  print (ex3 9922331)
  print (ex4 [1..50])
  print (ex5 30)
  print (ex7 "as vogais foram trocadas!")
  print (ex8 150)
  print (ex9 (+) [1, 2, 3] [10, 11, 12, 13, 14, 15])