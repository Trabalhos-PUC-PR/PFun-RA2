module Main where
import Ex1 ( ex1 )
import Ex2 ( ex2 )
import Ex3 ( ex3 )
import Ex4 ( ex4 )
import Ex5 ( ex5 )
import Ex6 ( ex6 )
import Ex7 ( ex7 )
import Ex8 ( ex8 )
import Ex9 ( ex9 )

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

main::IO()
main = do
  print (ex1)
  print (ex2 100_000)
  print (ex3 9_999_999_999)
  print (ex4 [1..10])
  print (ex5 100)
  print (take 15 (ex6 20))
  print (ex7 "as vogais foram trocadas!")
  print (ex8 123)
  print (ex9 (+) [1, 2, 3] [10, 11, 12, 13, 14, 15])

  