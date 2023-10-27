module Ex7 where

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