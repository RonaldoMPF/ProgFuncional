{-
- Dada uma tupla, divide o primeiro pelo segundo usando pattern matching.
- Ela deve ser indefinida quando o denominador for zero.
-}
divTuple (x, 0) = undefined
divTuple (x, y) = x / y

{-
 - Calcula o somatorio entre dois numeros a e b (a < b). Procure usar alguma funcao pronta sobre listas.
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorio a b = sum [a..b]

{-
 - Calcula o somatorio (recursivo) entre dois numeros a e b (a < b).
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorioRec a b | a == b = b
somatorioRec a b | a /= b = a + somatorioRec (a+1) b

-- Defina a funcao que eleva um membro ao quadrado
square x = x * x

-- Soma os quadrados de dois numeros.
sumSquares x y = square x + square y

-- Defina uma funcao de alta ordem que aceita uma função (Int -> Int) e aplica a funcao a dois numeros
normalSum a b = a + b

higherOrderSum f a b = f a b

-- Defina a soma dos quadrados em termos de higherOrderSum
hoSumSquares a b = higherOrderSum sumSquares a b

--Implemente a funcao mapFilter que primeiro aplica o map de uma funcao f a uma lista e depois aplica a funcao filter
-- a lista resultante. Procure usar a composicao de funcoes

{- Map e Filter feitos a partir de compreessão de listas-}
myMapComp f xs = [f x | x <- xs ]
myFilterComp p xs = [x | x <- xs, p x ]

{-Map e Filter feitos de maneira recursiva -}
myMapRec f [] = []
myMapRec f (x:xs) = f x : myMapRec f xs

myFilterRec p [] = []
myFilterRec p (x:xs) | p x == True =  x : myFilterRec p xs
                     | p x == False = myFilterRec p xs

mapFilter f p xs = myFilterComp p list
                  where list = myMapComp f xs
