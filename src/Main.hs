module Main where
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
fiboGenerator :: Int -> [Int]
fiboGenerator n = fibo n [0, 1]

last2:: [a] -> a
last2 list = head(tail(reverse list))

-- target, list
fibo :: Int -> [Int] -> [Int]
fibo target list
  | length list == target = list
  | otherwise = fibo target (list++[last list + last2 list])

  -- 3. Fatorar um número em seus divisores primos é uma tarefa importante para a segurança de dados.
  -- Escreva uma função que devolva os fatores primos de qualquer inteiro dado maior que
  -- 100.000.000 e menor que 1.000.000.000.

  -- 4. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos
  -- quadrados e o quadrado da soma destes inteiros.

  -- 5. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função
  -- que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores
  -- que um determinado inteiro dado.

  -- 6. Escreva uma função, usando iterate que devolva uma lista infinita de inteiros de tal forma que o
  -- inteiro 𝑛 será o dobro do inteiro 𝑛 − 1. Esta função deve receber o valor inicial da lista.

  -- 7. Escreva uma função que receba uma string e devolva outra string com as vogais trocadas. De tal
  -- forma que: 𝑎 será transformado em 𝑢; 𝑒 será transformado em 𝑜; 𝑖 não será transformado; 𝑜 será
  -- transformado em 𝑒 e 𝑢 será transformado em 𝑎;

  -- 8. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os
  -- números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro
  -- dado como argumento.

  -- 9. Escreva uma função análoga a função map, disponível no prelude, na qual um predicado, função,
  -- seja aplicado a duas listas e não a apenas uma. Por exemplo 𝑚𝑎𝑝2 (+) [1,2,3] [10,11,12] deve
  -- retornar [11,13,15]

main::IO()
main = do
  print ex1
  print (fiboGenerator 50)
