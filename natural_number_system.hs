--VAMOS CRIAR UM PROGRAMA QUE IMPLEMENTA OS AXIOMAS DE PEANO E AS OPERAÇÕES DE SOMA E MULTIPLICAÇÃO DE NÚMEROS NATURAIS
--O PRESENTE PROGRAMA IMPLEMENTA APENAS A LÓGICA, PORÉM DEFININDO-SE UM SISTEMA DE ESCRITA DE NÚMEROS PODE-SE REALIZAR CÁLCULOS COM O SISTEMA DE NUMERAÇÃO ASSIM DEFINIDO

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
predecessor Zero = error "Não existe o predecessor de zero no conjunto dos números naturais.\n"
predecessor (Sucessor x) = x

--4º axioma: Dois números possuem o mesmo sucessor se e somente se eles forem iguais
igualdade::Natural->Natural ->Bool
igualdade x y
    |Sucessor x Prelude.== Sucessor y = True
    |Sucessor x /= Sucessor y = False

--5ºaxioma: O conjunto dos números que começam com zero e são obtidos via operação de sucessão são os únicos números naturais existentes. (Isto equivale a afirmação de que o elemento zero é unico, uma vez que a operação de sucessão é única pelo axioma 4.)
instance Eq Natural where
    Zero == Zero = True
    Zero == _  = False
    _ == Zero = False




--SOMA
--Para definir a soma usamos uma relação recursiva
soma::Natural->Natural->Natural
soma x Zero = x
soma Zero x = x
soma x (Sucessor Zero) = Sucessor x --Se x é natural x+1= sucessor(x)
soma (Sucessor Zero) x = Sucessor x
soma x (Sucessor y) = Sucessor(soma x y)

--MULTIPLICAÇÃO
--Usamos a operação de soma para definir a operação de multiplicação recursivamente
multiplicacao::Natural ->Natural ->Natural
multiplicacao _ Zero = Zero
multiplicacao Zero _ = Zero
multiplicacao x (Sucessor Zero) = x -- Elemento neutro da multiplicação
multiplicacao (Sucessor Zero) x = x -- Elemento neutro da multiplicação
multiplicacao x (Sucessor y) = soma x (multiplicacao x y) 

--SISTEMA DE ESCRITA DOS NÚMEROS
--Função recursiva para escrever os algarismos usados para escrever o sistema de números decimais
--Definindo essa função recursivamente todos os números naturais podem ser escritos
write_digits :: Natural -> IO ()
write_digits Zero = putStrLn "0"
write_digits (Sucessor Zero) = putStrLn "1"
write_digits (Sucessor (Sucessor Zero)) = putStrLn "2"
write_digits (Sucessor (Sucessor (Sucessor Zero))) = putStrLn "3"
write_digits (Sucessor (Sucessor (Sucessor (Sucessor Zero)))) = putStrLn "4"
write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero))))) = putStrLn "5"
write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero)))))) = putStrLn "6"
write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero))))))) = putStrLn "7"
write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero)))))))) = putStrLn "8"
write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero))))))))) = putStrLn "9"


--FUNÇÃO PRINCIPAL
main::IO()
main = do
    putStrLn("Digitos usados para representar os números naturais:")
    write_digits Zero
    write_digits (Sucessor Zero) 
    write_digits (Sucessor (Sucessor Zero)) 
    write_digits (Sucessor (Sucessor (Sucessor Zero)))
    write_digits (Sucessor (Sucessor (Sucessor (Sucessor Zero)))) 
    write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero)))))
    write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero)))))) 
    write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero))))))) 
    write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero))))))))
    write_digits (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor (Sucessor Zero)))))))))

    putStrLn("Operações matemáticas nesse sistema:")
    putStrLn("1+1=2")
    putStrLn(show(soma (Sucessor Zero) (Sucessor Zero) ))
    putStrLn("2 x 2 = 4")
    putStrLn(show(multiplicacao (Sucessor (Sucessor Zero))  (Sucessor (Sucessor Zero))  ))
    
    putStrLn("Operação de sucessão e predecessão:")   
    putStrLn("Zero: "++show zero)
    putStrLn("Sucessor de zero: "++show (sucessor zero))
    putStrLn("Predecessor do sucessor de zero: "++show (predecessor (Sucessor zero)))
    putStrLn("Predecessor de zero: "++show (predecessor zero))
