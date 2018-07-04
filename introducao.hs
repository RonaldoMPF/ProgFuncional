{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = (a && not b) || (not a && b)
impl a b =  not a || b
equiv a b = impl a b && impl b a

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y
-}
pow 0 0 = error "Not determined!"
pow x 0 = 1
pow 0 y = 0
pow x y | y > 0 = x * (pow x (y-1))
        | y < 0  = 1 / (pow x (-y))


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero
-}
fatorial 0 = 1
fatorial x | x > 0 = x * fatorial(x-1)
           | otherwise = error "Negative number!"

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}


isPrime n | n <= 1 = False
          |otherwise = isPrime' n list
          where list = [2.. (n-1)]


isPrime' n [] = True
isPrime' n (x:xs) | n `mod` x == 0 = False
                  | otherwise = isPrime' n xs

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas.
-}
fib 0 = 0
fib 1 = 1
fib n | n > 0 = fib (n-1) + fib (n-2)
      | otherwise = undefined

fastFib 0 = 0
fastFib 1 = 1
fastFib n = fastFib' [0, 1] (n+1) 2

fastFib' (x:xs) n count | count == n = last xs
                        | otherwise = fastFib' ((x:xs) ++ [(listLast + beforeLast)]) n (count+1) where
                          listLast = last xs
                          beforeLast = last (init (x:xs))

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides.
-}
mdc x y | remainder == 0 = y
        | otherwise = mdc y remainder
        where remainder = x `mod` y

{-
- Calcula um MMC de dois numeros.
-}

mmc x y | x `mod` y == 0 = y
        | y `mod` x == 0 = x
        |otherwise =  (x * y) `div` mdcresult
        where mdcresult = mdc x y


{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True
-}
coprimo x y | x `mdc` y == 1 = True
            |otherwise = False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [(y, z) | y <- filter isPrime [1..(x-1)], z <- filter isPrime [1.. (x-1)], z + y == x]
