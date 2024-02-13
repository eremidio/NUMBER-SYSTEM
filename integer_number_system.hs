--VAMOS IMPLEMENTAR A CLASSE DOS NÚMEROS INTEIROS EM HASKELL
{-
ADOTAMOS COMO PONTO DE PARTIDA QUE AS OPERAÇÕES DE ADIÇÃO E MULTIPLICAÇÃO SOBRE O CONJUNTO DOS NATURAIS SÃO BEM DEFINIDAS.
VAMOS DEFINIR O CONJUNTO DOS INTEIROS COMO UM PAR ORDENADO (a,b) TAL QUE DOIS NÚMEROS (a,b) E (c, d) SÃO IGUAIS SE E SOMENTE SE a+d=b+c.
CADA CLASSE DE EQUIVALÊNCIA DEFINE UM NÚMERO INTEIRO.

SE a>b, ENTÃO EXISTE UM NATURAL c, TAL QUE a=b+c, USAMOS O SÍMBOLO c PARA REPRESENTAR O INTEIRO (a, b) E (b, a) É REPRESENTADO PELO SÍMBOLO -c.

O ZERO (a, b) CORRESPONDE AO CASO a=b, NATURALMENTE TEMOS QUE c+(-c)=0.
A OPERAÇÃO DE SUBTRAÇÃO É DEFINIDA COMO a-b=a+(-b).
-}

--TIPOS
--Definindo números inteiros como um par ordenado de naturais
data Inteiro = Inteiro (Int, Int)
    deriving (Show)

--FUNÇÕES

--Função que representa um número em notação decimal (z deve ser inicializado em 0 ao se invocar esta função)
repr::Inteiro ->Int->Int
repr (Inteiro (x, y)) z
    |x==y = z
    |x > y = repr(Inteiro (x, y+1)) (z+1)
    |x < y = (-1)*repr (Inteiro(y, x)) z --Introduzindo o o símbolo (-) para denotar o elemento simétrico da adição
    


--Função que determinar se dois números inteiros são iguais ou não
int_equal::Inteiro->Inteiro->Bool
int_equal (Inteiro (x1, x2))(Inteiro (y1, y2))
     |x1+y2==x2+y1 = True
     |x1+y2/=x2+y1 = False        

--Função que adiciona dois inteiros
int_soma::Inteiro->Inteiro->Inteiro
int_soma (Inteiro (x1, x2))(Inteiro (y1, y2)) = Inteiro (x1+y1, x2+y2)

--Função que subtrai dois inteiros
int_subtracao::Inteiro->Inteiro->Inteiro
int_subtracao (Inteiro (x1, x2))(Inteiro (y1, y2)) = Inteiro (x1+y2, x2+y1)

--Função que calcula o inverso aditivo de um inteiro
inverso::Inteiro->Inteiro
inverso (Inteiro (x, y)) = Inteiro(y, x)


--FUNÇÃO PRINCIPAL
main::IO()
main = do
    putStrLn("(2, 3) == (4, 5)"++ show(int_equal (Inteiro(2, 3)) (Inteiro(6, 5)) ))
    putStrLn("(5, 3) == (7, 5)"++ show(int_equal (Inteiro(5, 3)) (Inteiro(7, 5)) ))
    putStrLn("(2, 3) + (7, 5) = "++ show(int_soma (Inteiro(2, 3)) (Inteiro(7, 5)) ))
    putStrLn("(5, 3) + (7, 5) = "++ show(int_soma (Inteiro(5, 3)) (Inteiro(7, 5)) ))
    putStrLn("(10, 3) - (7, 5) = "++ show(int_subtracao (Inteiro(10, 3)) (Inteiro(7, 5)) ))
    putStrLn("(5, 3) - (15, 6) = "++ show(int_subtracao (Inteiro(5, 3)) (Inteiro(15, 6)) ))
    putStrLn("Inverso de (10, 3) = "++show(inverso (Inteiro(10, 3)) ))
    putStrLn("(10, 3) é representado como: "++ show(repr (Inteiro(10,3)) 0))
    putStrLn("(3, 10) é representado como: "++ show(repr (Inteiro(3,10)) 0))
    putStrLn("(7, 3) é representado como: "++ show(repr (Inteiro(7,3)) 0))
    putStrLn("(3, 7) é representado como: "++ show(repr (Inteiro(3,7)) 0))
    putStrLn("(7, 7) é representado como: "++ show(repr (Inteiro(7,7)) 0))
    putStrLn("(3, 3) é representado como: "++ show(repr (Inteiro(3,3)) 0))
