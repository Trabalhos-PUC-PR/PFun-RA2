module Ex3 where
import GHC.Float.RealFracMethods (floorFloatInt)
import Data.List (sort)

-- 3. Fatorar um número em seus divisores primos é uma tarefa importante para a segurança de dados.
-- Escreva uma função que devolva os fatores primos de qualquer inteiro dado maior que
-- 100.000.000 e menor que 1.000.000.000.

-- Find the square root of the integer number n and round down to the closest whole number. Let's call this number s.
-- Start with the number 1 and find the corresponding factor pair: n ÷ 1 = n. So 1 and n 
-- -- are a factor pair because division results in a whole number with zero remainder.
-- Do the same with the number 2 and proceed testing all integers (n ÷ 2, n ÷ 3, n ÷ 4... n ÷ s) up through the square 
-- -- root rounded to s. Record the factor pairs where division results in whole integer numbers with zero remainders.
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