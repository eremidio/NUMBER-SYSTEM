--VAMOS CRIAR UM PROGRAMA EM  HASKELL PARA ENCONDAR NÚMEROS REAIS EM BINÁRIOS
--EXECUTAR ESTE SCRIPT COM O COMANDO: runghc real_number_system_binary_representation.hs


{--

OS NÚMEROS REAIS FORMAM UM CONJUNTO NÃO ENUMERÁVEL QUE POSSUI A CARDINALIDADE DO CONTÍNUO, POR ESTA RAZÃO NÃO SE PODE INDEXAR
ELEMENTOS DESTE CONJUNTO USANDO A NOTAÇÃO {r(0), r(1), ...}. ENTRE DOIS NÚMEROS REAIS QUAISQUER EXISTE SEMPRE PELO MENOS UM
NÚMERO REAL.

AINDA ASSIM É POSSÍVEL ASSOCIAR A CADA NÚMERO REAL UMA STRING BINÁRIA TANTO PARA A PARTE INTEIRA QUANTO PARA SUA PARTE
DECIMAL. PARA A PARTE DECIMAL COMPARA-SE O VALOR COM POTÊNCIA DE 2^(-n) PARA n=1,2,3,...


PARA MAIORES INFORMAÇÕES: https://www.youtube.com/watch?v=iaUwNuaSLUk

--} 

--CONSTANTE
zero::Double
zero = 0.0

one::Double
one = 1.0

--FUNÇÕES
--Função que extrai a parte inteira de um número real
get_integer_part::Double->Integer
get_integer_part x = floor x

--Função que extrai a parte decimal de um número real
get_decimal_part::Double->Double
get_decimal_part x = x - fromInteger(floor x)


--Função que representa um inteiro como uma string binária
int_to_binary::Integer->[Char]
int_to_binary 0 = ""
int_to_binary n = int_to_binary (n `div` 2) ++ [if n `mod` 2 == 1 then '1' else '0']


from_int_to_binary::Integer->IO()
from_int_to_binary 0 = putStrLn "0"  -- Caso base
from_int_to_binary n = putStrLn (int_to_binary n)



--Função que representa um número real entre 0 e 1 como uma string binária
float_to_binary :: Double -> Double -> Double -> Int -> IO ()
float_to_binary x a b n
  | x == 0.0 || n == 0 = putStrLn "0"  --Caso base
  | abs (x - (a + b) / 2) < 1e-10 = putStrLn "1_..."  -- Caso base
  | x > (a + b) / 2 = do
      putStr "1"
      float_to_binary x ((a + b) / 2) b (n - 1)  -- Recursão
  | x < (a + b) / 2 = do
      putStr "0"
      float_to_binary x a ((a + b) / 2) (n - 1)  -- Recursão


--FUNÇÃO PRINCIPAL
main::IO()
main = do 

  --Recebendo input do usuário
  putStr("[Número real (casas decimais separadas por um ponto)] x: ")
  x_<-getLine
  let x= read x_::Double

  --Extraindo as partes decimais e inteiras de um natural
  let int_x = get_integer_part x
  let float_x = get_decimal_part x

  putStrLn("[Parte inteira]  [x]: "++ show int_x)
  putStrLn("[Parte decimal]  x-[x]: "++ show float_x)
  

  --Escrevendo o número como uma string binária 
  putStr("[String binária associada a parte inteira]  [x]: ")
  from_int_to_binary int_x

  putStr("[String binária associada a parte decimal]  x-[x]: ")
  float_to_binary float_x zero one 50 



