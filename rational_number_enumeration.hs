--VAMOS CRIAR UM PROGRAMA QUE ORDENA O CONJUNTO DOS NÚMEROS RACIONAIS

{-

O CONJUNTO DOS RACIONAIS É HOMEOMORFÍCO AO CONJUNTO DE PARES ORDENADOS CONTENDO ENTRADAS INTEIRAS, SENDO ASSIM UM CONJUNTO ENUMERÁVEL.
EXISTEM VÁRIAS FORMAS DE DEFINIR UMA BIJEÇÃO DO CONJUNTO DOS RACIONAIS NO CONJUNTO DOS NATURAIS.

POR EXEMPLO, PODE-SE TOMAR A FUNÇÃO f:Q->N, DEFINDA PARA UMA RACIONAL q=a/b, COMO f(q)=(2^a)(3^b), TAL FUNÇÃO É BIJETIVA. A FUNÇÃO É
INVERTÍVEL, POR CONTA DO TEOREMA FUNDAMENTAL DA ARITMÉTICA ACERCA DA DECOMPOSIÇÃO ÚNICA DE UM INTEIRO EM FATORES PRIMOS.

OUTRA POSSIBILIDADE SERIA TOMAR A FUNÇÃO DE PAREAMENTO DE CANTOR DEFINIDA PELA RELAÇÃO π(q)= π(a, b)={(a+b)(a+b+1)/2}+b. TAL FUNÇÃO É 
BIJETIVA E INVERTÍVEL.

PARA MAIORES INFORMAÇÕES: https://github.com/eremidio/NUMBER-THEORY-ALGORITHMS/blob/main/cantor_pairing_function.h

-}


--IMPORTANDO BIBLIOTECAS
import Data.List (sort, sortBy)
import Data.Ord (comparing)


--FUNÇÕES AUXILIARES
--Função de pareamento de Cantor
cantor_pairing::Integer->Integer->Integer
cantor_pairing a b = div ((a+b)*(a+b+1)) 2 + b


--TIPO
data Racional = Racional Integer Integer
    deriving(Eq)

--Overloading do operador de comparação usando a função de pareamento de Cantor
instance Ord Racional where
  compare (Racional a b) (Racional c d) = compare (cantor_pairing a b) (cantor_pairing c d)

--Formatação da exibição de elementos da classe dos racionais
instance Show Racional where
  show (Racional a b) = show a ++ "/" ++ show b


--FUNÇÕES
--Função que gera uma lista de Racionais
generate_rational_list::Integer->[Racional]
generate_rational_list n = [Racional i j | i<-[0..n], j<-[1..n]]


--Função que enumera uma lista de racionais
enumerate_rational_list::[Racional]->[(Integer, Racional)]
enumerate_rational_list rational_list = zip [0..] rational_list

--PROGRAMA PRINCIPAL
main = do
  --Recebendo input do usuário
  putStr("[Inteiro] n: ")
  n_<-getLine
  let n = read n_::Integer

  ----Procedimentos
  --Gerando uma lista de Racionais
  let rationals = generate_rational_list n
  
    
  --Enumeracão da lista de Racionais usando a função de pareamento de Cantor
  putStrLn("Lista de números racionais enumerada: ")
  let enumerate_rationals = enumerate_rational_list rationals
  print enumerate_rationals

  


