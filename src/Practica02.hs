module Practica02 where

import Prelude hiding (map, filter, reverse)

-- BINARIOS
data Bit = O | I 
    deriving (Show, Eq)

type Binario = [Bit]

-- Función auxiliar: Convierte un Bit a su valor Int
bitToInt :: Bit -> Int
bitToInt O = 0
bitToInt I = 1

-- Función auxiliar: Convierte Int (0 o 1) a Bit
intToBit :: Int -> Bit
intToBit 0 = O
intToBit 1 = I
intToBit _ = error "Valor no binario"

-- Función auxiliar: Invierte la lista (para emular 'reverse')
listToInt :: [a] -> [a]
listToInt [] = []
listToInt (x:xs) = listToInt xs ++ [x]


-- toDecimal: Binario -> Int
-- Convierte una representación binaria a decimal.
toDecimal :: Binario -> Int
toDecimal [] = 0
toDecimal (bit : bs) = bitToInt bit * base + toDecimal bs
  where 
    -- Definimos 'base' como variable local
    base = 2 ^ (length bs)


-- Función auxiliar: Construye la lista binaria en orden inverso 
toBinAux :: Int -> [Bit] -> [Bit]
toBinAux 0 acc = acc
toBinAux m acc = toBinAux (m `div` 2) (intToBit (m `mod` 2) : acc)

-- toBin: Int -> Binario
-- Convierte un número decimal a su representación binaria 
toBin :: Int -> Binario
toBin 0 = [O]
toBin n
    | n < 0     = error "toBin no soporta números negativos"
    | otherwise = listToInt (toBinAux n []) -- Invertimos el resultado de la auxiliar
  where
    -- toBinAux es una función auxiliar definida localmente (usando where para definir funciones)
    toBinAux 0 acc = acc
    toBinAux m acc = toBinAux (m `div` 2) (intToBit (m `mod` 2) : acc)


-- Función auxiliar: Extraer cabeza y cola, usando O si la lista está vacía
getHeadAndTail :: Binario -> (Bit, Binario)
getHeadAndTail [] = (O, [])
getHeadAndTail (b:bs) = (b, bs)

-- Función auxiliar: Suma las listas al revés 
sumaBinAux :: Binario -> Binario -> Bit -> Binario
sumaBinAux [] [] O = []
sumaBinAux [] [] I = [I]
sumaBinAux bs1 bs2 carry = bit : sumaBinAux bs1' bs2' newCarry
  where
    -- Definiciones locales para claridad 
    (b1Head, bs1') = getHeadAndTail bs1
    (b2Head, bs2') = getHeadAndTail bs2
    s = bitToInt b1Head + bitToInt b2Head + bitToInt carry
    bit = intToBit (s `mod` 2)
    newCarry = intToBit (s `div` 2)

-- suma: Binario -> Binario -> Binario
-- Suma dos números binarios (orden normal)
suma :: Binario -> Binario -> Binario
suma b1 b2 = listToInt (sumaBinAux (listToInt b1) (listToInt b2) O)


-- LISTAS

-- palindromo: [a] -> Bool
-- Comprueba si una lista es un palíndromo.
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == listToInt xs -- Compara la lista con su versión invertida

-- Función auxiliar: Devuelve los elementos de lista1 que NO están en lista2 (lista1 \ lista2)
quitarElementos :: Eq a => [a] -> [a] -> [a]
quitarElementos [] _ = []
quitarElementos (x:xs) ys
    | x `elem` ys = quitarElementos xs ys -- Si está en ys, lo ignoramos
    | otherwise   = x : quitarElementos xs ys -- Si NO está en ys, lo incluimos

-- diferenciaSimetrica: [a] -> [a] -> [a]
-- Elementos en la unión, pero no en la intersección.
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = quitarElementos xs ys ++ quitarElementos ys xs


-- Función auxiliar: agrega un elemento a cada lista de una lista de listas (para conjuntoPotencia)
agregarElementos :: a -> [[a]] -> [[a]]
agregarElementos _ [] = []
agregarElementos x (ys:yss) = (x:ys) : agregarElementos x yss

-- conjuntoPotencia: [a] -> [[a]]
-- Calcula el conjunto potencia de una lista.
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) =  [ (x:ys) | ys <- conjuntoPotencia xs ] ++ conjuntoPotencia xs


-- LISTAS DE LONGITUD PAR
type ListaPar a b = [(a,b)]

-- longitud: ListaPar a b -> Int
-- Devuelve la longitud total de la lista de pares (dos veces el número de pares).
longitud :: ListaPar a b -> Int
longitud [] = 0
longitud (_:xs) = 2 + longitud xs

-- myMap: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d
-- Aplica una función al primer elemento de cada par y otra al segundo.
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap _ _ [] = []
myMap fa fb ((x, y) : xys) = (fa x, fb y) : myMap fa fb xys


-- sumaPares: ListaPar a b -> (a,b)
-- Suma las primeras entradas de los pares y las segundas.
sumaPares :: Num a => Num b => ListaPar a b -> (a,b)
sumaPares [] = (0, 0)
sumaPares ((x, y) : xys) = (x + sumX, y + sumY)
  where
    -- Definición local del resultado recursivo 
    (sumX, sumY) = sumaPares xys


-- myFilter: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
-- Filtra los pares que cumplen una condición.
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter _ [] = []
myFilter p (xy : xys)
    | p xy      = xy : myFilter p xys
    | otherwise = myFilter p xys