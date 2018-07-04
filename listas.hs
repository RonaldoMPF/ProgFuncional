{-
- Encontra o ultimo elemento de uma lista. Caso a lista seja vazia retorne o seguinte comando: error "Lista vazia!"
-}
meuLast [] = error "Empty List!"
meuLast xs = last xs

{-
- Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo"
-}
penultimo [] = error "Lista sem penultimo!"
penultimo [x] = error "Lista sem penultimo!"
penultimo xs = last initList
              where initList = init xs

{-
- Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}
elementAt k [] = error "Empty List!"
elementAt 1 (x:xs) = x
elementAt k (x:xs) | k > meuLength (x:xs) = error "List index out of range!"
                   |k <= meuLength(x:xs) = elementAt' 1 k (x:xs)

elementAt' i k (x:xs) | i == k = x
                      | otherwise = elementAt' (i+1) k xs
{-
- Retorna o tamanho de uma lista.
-}
meuLength [] = 0
meuLength (x:xs) = 1 + meuLength xs

{-
- Retorna o inverso de uma lista.
-}
meuReverso [] = []
meuReverso (x:xs) = meuReverso (xs) ++ [x]

{-
- Diz se uma lista é palindrome.
-}

isPalindrome [] =  True
isPalindrome [x] = True
isPalindrome xs | xs == meuReverso xs = True
                | otherwise = False

{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
-}
compress [] = []
compress xs | elem lasElement inicialList == True = compress inicialList
            | otherwise = compress inicialList ++ [lasElement] where
              lasElement = last xs
              inicialList = init xs

{- Conta ocorrencias de um elemento em uma lista-}
countOcurrences k [] = 0
countOcurrences k (x:xs) | k == x = 1 + countOcurrences k xs
                         | otherwise = countOcurrences k xs
{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
compact [] = []
compact xs = compact' compresedList xs where
  compresedList = compress xs

compact' [] ys = []
compact' (x:xs) ys = replicate k x ++ compact' xs ys where
  k = countOcurrences x ys



{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
encode :: [Int] -> [(Int, Int)]
encode [] = []
encode xs = [ (x, countOcurrences x xs) | x <- compresedList] where
  compresedList = compress xs

{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}
split :: [Int] -> Int -> [[Int]]
split [] i = error "Empty List!"
split xs 0 = [[], xs]
split xs i | i > meuLength xs = error "List index out of range!"
           | otherwise = [ [ x | x <- xs, indexof x xs <= i], [ y | y <- xs, indexof y xs > i] ]

indexof :: Int -> [Int] -> Int
indexof n [] = error "Empty List!"
indexof n (x:xs) | n == x = 1
                 | otherwise = 1 + indexof n xs
{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo.
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}
slice :: [Int] -> Int -> Int -> [Int]
slice xs imin imax = [ x | x <- xs, (indexof x xs) <= imax, (indexof x xs) >= imin]


{-
- Insere um elemento em uma posicao especifica de uma lista.
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}
insertAt :: Int -> Int -> [Int] -> [Int]
insertAt el pos [] = []
insertAt el 1 xs = (el : xs)
insertAt el pos xs = insertAt' el pos xs 1

insertAt' :: Int -> Int -> [Int] -> Int -> [Int]
insertAt' el pos [] index = []
insertAt' el pos (x:xs) index | index == pos =  (el: x : insertAt' el pos xs (index+1))
                              | otherwise = (x : insertAt' el pos xs (index+1))


{-
- Ordena uma lista em ordem crescente. Voce deve seguir a ideia do selectionsort onde os elementos
- menores sao trazidos para o inicio da lista um a um. Esta funcao ja esta implementada.
-}
minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys
    where
        x = minList xs
        ys = sort (remove x xs)

{-
- Calcula a soma de todos os elementos de uma lista usando foldr.
-}
mySum xs = undefined

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}
maxList xs = undefined

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse.
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1].
-}
buildPalindrome xs = undefined

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}
mean xs = undefined

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr.
-}
myAppend xs ys = undefined
