--VAMOS CRIAR UM PROGRAMA EM HASKELL PARA REPRESENTAR OS NÚMEROS RACIONAIS 

{-
ADOTAMOS COMO PONTO DE PARTIDA QUE O CONJUNTO DOS NÚMEROS INTEIROS ESTÁ PLENAMENTE ESTABELECIDO BEM COMO AS OPERAÇÕES DE SOMA, MULTIPLICAÇÃO E SUBTRAÇÃO. UM NÚMERO RACIONAL É DEFINIDO COMO UM PAR ORDENADO (a, b) DEFINIDO POR UMA RELAÇÃO DE EQUIVALÊNCIA. TAL QUE  (a, b) E (c, d) SÃO EQUIVALENTES SE E SOMENTE SE (a*d)=(b*c). A MULTIPLICAÇÃO E DIVISÃO SÃO DEFINIDAS POR (a, b)*(c, d)=(ac, bd) E (a, b)/(c, d)=(ad, bc). O INVERSO MULTIPLICATIVO DE (a, b) É DENOTADO POR (b, a). A SOMA E SUBTRAÇÃO SÃO DADOS POR (a, b)+(c, d)=(ad+bc, bd), (a, b)-(c, d)=(ad-bc, bd), COM a, b, c, d INTEIROS.

-}

--TIPO
data Racional = Racional (Integer, Integer)
    deriving(Show)

--FUNÇÕES
--Comparando dois números racionais
rac_igual::Racional->Racional->Bool
rac_igual (Racional(a, b)) (Racional(c, d))
    | a*d==b*c = True
    | a*d/=b*c = False

--Multiplicação
multiplicacao::Racional->Racional->Racional
multiplicacao (Racional(a, b)) (Racional(c, d)) = Racional(a*c, b*d)

--Inverso
inverso::Racional->Racional
inverso (Racional (a, b)) = Racional(b, a)

--Divisão
divisao::Racional->Racional->Racional
divisao (Racional(a, b)) (Racional(c, d)) = Racional(a*d, b*c)

--Adição
soma::Racional->Racional->Racional
soma (Racional(a, b)) (Racional(c, d)) = Racional (a*d+b*c, b*d)

--Subtração
subtracao::Racional->Racional->Racional
subtracao (Racional(a, b)) (Racional(c, d)) = Racional (a*d-b*c, b*d)
 
--Função que calcula a parte inteira da divisão de dois números inteiros positivos (usar k = 0 ao invocar esta função)
div_int::Integer->Integer->Integer->Integer
div_int n m k
    |n<m = k
    |n>=m = div_int (n-m) m (k+1)


--Função que calcula o resto da divisão de dois números inteiros positivos
mod_int::Integer->Integer->Integer
mod_int n m 
    |n<m = n
    |n>=m = mod_int (n-m) m 


-- Representação decimal de um racional (a menos de um sinal multiplicativo)
decimal :: Racional -> Integer -> Integer -> IO ()
decimal (Racional (a, b)) z n
    | z == n = putStr "..."
    | z < n = do
        let parte_inteira = div_int a b 0
            parte_decimal = mod_int a b
        putStr (show parte_inteira ++",")
        decimal (Racional (parte_decimal*10, b)) (z+1) n

--FUNÇÃO PRINCIPAL
main::IO()
main = do
    putStrLn("(2, 3) == (4, 6) "++ show(rac_igual (Racional(2, 3)) (Racional(4, 6)) ))
    putStrLn("(5, 3) == (7, 5) "++ show(rac_igual (Racional(5, 3)) (Racional(7, 5)) ))
    putStrLn("(2, 3) * (4, 6) = "++ show(multiplicacao (Racional(2, 3)) (Racional(4, 6)) ))
    putStrLn("(5, 3) * (7, 5) = "++ show(multiplicacao (Racional(5, 3)) (Racional(7, 5)) ))
    putStrLn("(2, 3) / (4, 6) = "++ show(divisao (Racional(2, 3)) (Racional(4, 6)) ))
    putStrLn("(5, 3) / (7, 5) = "++ show(divisao (Racional(5, 3)) (Racional(7, 5)) ))
    putStrLn("(2, 3) + (4, 6) = "++ show(soma (Racional(2, 3)) (Racional(4, 6)) ))
    putStrLn("(5, 3) + (7, 5) = "++ show(soma (Racional(5, 3)) (Racional(7, 5)) ))
    putStrLn("(2, 3) - (4, 6) = "++ show(subtracao (Racional(2, 3)) (Racional(4, 6)) ))
    putStrLn("(5, 3) - (7, 5) = "++ show(subtracao (Racional(5, 3)) (Racional(7, 5)) ))
    putStr("(18, 3) = ") 
    decimal (Racional(18, 3)) 0 1
    putStr("\n")
    putStr("Digitos da representação decimal de (5, 3) ou 5/3: ")
    decimal (Racional(5,3)) 0 20
    putStr("\n")
    putStr("Digitos da representação decimal de (3, 5) ou 3/5: ")
    decimal (Racional(3,5)) 0 5
    putStr("\n")
    putStr("Digitos da representação decimal de (4, 7) ou 4/7: ")
    decimal (Racional(4,7)) 0 15
    putStr("\n")
