{--
VAMOS CRIAR UMA OPERAÇÃO MATEMÁTICA NO CONJUNTO DOS NÚMEROS INTEIROS, RACIONAIS E REAIS QUE POSSUEM PROPRIEDADES INTERESSANTES SIMILARES AS OPERAÇÕES ARITMÉTICAS
ELEMENTARES. UMA INTERESSANTE APLICAÇÃO DE CONCEITOS ELEMENTARES DE OPERAÇÕES ARÍTMETICAS EMCONJUNTOS DE SISTEMAS NUMÉRICOS
--}

{--
A OPERAÇÃO BOLINHA PARA DOIS NÚMEROS INTEIROS, RACIONAIS E REAIS a, b É DEFINIDO COMO: a°b=(a+b)+(ab).
ESTA OPERAÇÃO NESTES CONJUNTOS SATISFAZEM A SEGUINTE PROPRIEDADES:
1. COMUTATIVIDADE: a°b=(b°a)
2. ASSOCIATIVIDADE: (a°b)°c=a°(b°c)
3. EXISTE UM ELEMENTO NEUTRO 0: (a°0)=a

ESTA OPERAÇÃO NÃO SATISFAZ A PROPRIEDADE DE DISTRIBUTIVIDADE

--}

--EXECUTAR ESTE PROGRAMA COM O COMANDO: runghc bolinha.hs


--FUNÇÕES
--Definição da operação bolinha
bolinha::(Num a, Show a)=> a->a->a
bolinha x y = (x+y)+(x*y)



--Função principal
main::IO()
main=do
 --INTEIROS
 --Recebendo input do usuário
 putStr("Digite um número inteiro x: ")
 x_<-getLine
 let x= read x_::Integer
 putStr("Digite um número inteiro y: ")
 y_<-getLine
 let y= read y_::Integer
 putStr("Digite um número inteiro z: ")
 z_<-getLine
 let z= read z_::Integer

 --Calculando o resultado da operação °
 putStrLn("x°y = "++ (show (bolinha x y)))
 putStrLn("x°z ="++ (show (bolinha x z)))
 putStrLn("y°z ="++ (show (bolinha y z)))

 --Teste de comutatividade
 putStrLn("y°x = "++ (show (bolinha y x)))
 putStrLn("z°x ="++ (show (bolinha z x)))
 putStrLn("z°y ="++ (show (bolinha z y)))

 if((bolinha x y)== (bolinha y x) && (bolinha x z)== (bolinha z x) && (bolinha z y)== (bolinha y z))
  then putStrLn("Propriedade comutativa satisfeita!")
  else putStrLn("Propriedade comutativa não satisfeita!")

 --Teste de associatividade
 putStrLn("(x°y)°z = "++ (show (bolinha (bolinha x y) z)))
 putStrLn("x°(y°z) = "++ (show (bolinha x (bolinha y z))))

 if((bolinha (bolinha x y) z)== (bolinha x (bolinha y z)))
  then putStrLn("Propriedade associativa satisfeita!")
  else putStrLn("Propriedade associativa não satisfeita!")

--Teste do elemento neutro
 putStrLn("x°0 = "++ (show (bolinha x 0)))
 putStrLn("y°0 ="++ (show (bolinha y 0)))
 putStrLn("z°0 ="++ (show (bolinha z 0)))

