--VAMOS CRIAR UM PROGRAMA QUE IMPLEMENTA OS AXIOMAS DE PEANO E AS OPERAÇÕES DE SOMA E MULTIPLICAÇÃO DE NÚMEROS NATURAIS
--O PROGRAMA IMPLEMENTAR APENAS A LÓGICA, DEFININDO UM SISTEMA DE ESCRITA DE NÚMEROS PODE-SE REALIZAR CÁLCULOS COM O SISTEMA DE NUMERAÇÃO ASSIM DEFINIDO

--CRIANDO TIPOS

data Natural = Zero |Sucessor Natural --O construtor de um número natural é definido recursivamente via operação de sucessão
    deriving(Show, Ord)

--AXIOMAS
--1ª axioma: Zero é um número natural
zero::Natural
zero = Zero

--2ºaxioma: Todo número possui um sucessor que também é um número natural
sucessor::Natural->Natural
sucessor Zero = Sucessor Zero --(Sucessor Zero é hum)
sucessor x  = Sucessor x

--3ºaxioma: Zero não é sucessor de nenhum número natural  
predecessor::Natural -> Natural
predecessor Zero = error "Não existe o predecessor de zero no conjunto dos números naturais\n"
predecessor (Sucessor x) = x

--4º axioma: Dois números possuem o mesmo sucessor se e somente se eles forem iguais.
igualdade::Natural->Natural ->Bool
igualdade x y
    |Sucessor x Prelude.== Sucessor y = True
    |Sucessor x /= Sucessor y = False

--5ºaxioma: O conjunto inicializado dos números que começam com zero e são obtidos via operaçao de sucessão são os únicos números naturais existentes. (Isto equivale a afirmação de que o elemento zero é unico, uma vez que a operação de sucessão é única pelo axioma 4.)
instance Eq Natural where
    Zero == Zero = True
    Zero == _  = False
    _ == Zero = False




--SOMA
--Para definir a soma usamos a relação recursiva
soma::Natural->Natural->Natural
soma x Zero = x
soma Zero x = x
soma x (Sucessor Zero) = Sucessor x --Se x é natural x+1= sucessor(x)
soma (Sucessor Zero) x = Sucessor x
soma x (Sucessor y) = Sucessor(soma x y)

--MULTIPLICAÇÃO
--Usamos a operação de soma para definir a operação de soma
multiplicacao::Natural ->Natural ->Natural
multiplicacao _ Zero = Zero
multiplicacao Zero _ = Zero
multiplicacao x (Sucessor Zero) = x -- Elemento neutro da multiplicação
multiplicacao (Sucessor Zero) x = x -- Elemento neutro da multiplicação
multiplicacao x (Sucessor y) = soma x (multiplicacao x y) 

--FUNÇÃO PRINCIPAL
main::IO()
main = do
    putStrLn("Zero: "++show zero)
    putStrLn("Sucessor de zero: "++show (sucessor zero))
    putStrLn("Predecessor de zero: "++show (predecessor zero))
