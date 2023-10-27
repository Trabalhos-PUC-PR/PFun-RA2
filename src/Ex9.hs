module Ex9 where

-- 9. Escreva uma função análoga a função map, disponível no prelude, na qual um predicado, função,
-- seja aplicado a duas listas e não a apenas uma. Por exemplo 𝑚𝑎𝑝2 (+) [1,2,3] [10,11,12] deve
-- retornar [11,13,15]

ex9 :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
ex9 _ [] _ = []
ex9 _ _ [] = []
ex9 func param1 param2 = func(head param1) (head param2) : ex9 func (tail param1) (tail param2) 