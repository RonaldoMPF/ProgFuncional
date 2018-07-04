--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
pow = \x y -> x ^ y


fatorial 0 = (\x -> x) 1
fatorial x = (\y -> y * fatorial (y-1)) x

isPrime x | x <= 1 = False
          | otherwise = isPrime' x list where
            list = (\y -> [2..(y-1)]) x

isPrime' n [] = (\x -> x) True
isPrime' n (x:xs) | (\y z -> y `mod` z ) n x == 0 = False
                  | otherwise = isPrime' n xs

fib 0 = (\x -> x) 0
fib 1 = (\x -> x) 1
fib n = (\x -> fib(n-1) + fib(n-2)) n

lambdaMod = (\n1 n2 -> n1 `mod` n2 )

mdc x y | remainder == 0 = y
        | otherwise = mdc y remainder where
          remainder = lambdaMod x y

mmc x y | lambdaMod x y == 0 = x
        | lambdaMod y x == 0 = y
        | otherwise = (\n1 n2 -> ((n1 * n2) `div` (mdc n1 n2))) x y

coprimo x y | (\n1 n2 -> n1 `mdc` n1 ) x y == 1 = True
            | otherwise = False

goldbach x = (\x -> [ (y, z) | y <- filter isPrime [1..(x-1)], z <- filter isPrime [1..(x-1)], y + z == x ]) x

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

meuLast [] = error "lista Vazia!"
meuLast xs = (\x -> last x) xs

penultimo xs = undefined
elementAt i xs = undefined
meuLength xs = undefined
meuReverso xs = undefined
isPalindrome xs = undefined
compress xs = undefined
compact xs = undefined
encode xs = undefined
split xs i = undefined
slice xs imin imax = undefined
insertAt el pos xs = undefined
sort xs = undefined
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean xs = undefined
myAppend xs ys = undefined
