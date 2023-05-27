--VAMOS CRIAR UM PROGRAMA EM HASKELL QUE EXECUTA OPERAÇÕES DE DIVISÃO DE INTEIROS USANDO APENAS ADIÇÃO E SUBTRAÇÃO


--FUNÇÕES
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

--FUNÇÃO PRINCIPAL
main::IO()
main = do
    putStrLn("Digite o valor do dividendo:")
    n1<-getLine
    let num1= read n1::Integer
    putStrLn("Digite o valor do divisor:")
    n2<-getLine
    let num2= read n2::Integer
    putStrLn("Divisão de "++ show num1++" e "++show num2++": "++show(div_int num1 num2 0))
    putStrLn("Resto da divisão de "++ show num1++" e "++show num2++": "++show(mod_int num1 num2))
