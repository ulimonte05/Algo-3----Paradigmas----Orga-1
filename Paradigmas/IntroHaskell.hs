module IntroHaskell where
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Foldable (Foldable(fold))
{-# HLINT ignore "Use sum" #-}

sumaAux :: Num a => [a] -> a
sumaAux = foldr (+) 0


suma :: Num a => a -> a -> a
suma x y = sumaAux [x,y]

--pertenece :: Num a => a -> [a] -> Bool
--pertenece = foldr (\x xs -> x == e) [] == [e] 

concatenar :: [[a]] -> [a]
concatenar = foldr (++) []

concatenar' :: [a] -> [a] -> [a]
concatenar' xs ys = foldr (:) ys xs

elemAux :: Eq a => a -> [a] -> Bool
elemAux x = foldr (\y acc -> x == y || acc) False

filterAux :: (a -> Bool) -> [a] -> [a]
filterAux p = foldr (\x acc -> if p x then x : acc else acc) []

mapAux :: (a -> b) -> [a] -> [b]
mapAux f = foldr (\x acc -> f x : acc) []

listaPares :: [Integer] -> [Integer]
listaPares = filterAux even

todosMas1 :: [Integer] -> [Integer]
todosMas1 = mapAux (+1)

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x y -> if p x y then x else y)

maximo :: forall a. Ord a => [a] -> a
maximo = mejorSegun (>)

max2 ::(Ord a) => [a] -> a
max2 = foldr1 max

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = reverse (sumaParcialAux (reverse xs))

sumaParcialAux :: Num a => [a] -> [a]
sumaParcialAux [] = []
sumaParcialAux (x:xs) = foldr (+) 0 (x:xs) : sumaParcialAux xs

--sumaParcial :: Num a => [a] -> [a]
--sumaParcial [] = []
--sumaParcial (x:xs) = foldl (+) x xs : sumaParcial xs

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0
-- ?????

prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos (x:xs) = foldr (:) [] (reverse (x:xs)) : prefijos xs

--zip' :: [a] -> [b] -> [(a, b)]
--zip' [] [] = []
--zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                                then x : entrelazar xs []
                                else x : head ys : entrelazar xs (tail ys)

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                        then [x]
                                        else x : elementosEnPosicionesPares (tail xs)



recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e (x:xs) = recr (\x xs rec -> if x == e then xs else x : rec) [] (x:xs)

sacarUna2 :: Eq a => a -> [a] -> [a]
sacarUna2 _ [] = []
sacarUna2 e (x:xs)
    | e == x = xs
    | otherwise = x : sacarUna2 e xs

insertarOrdenado :: (Ord a) => a -> [a] -> [a]
insertarOrdenado e [] = [e]
insertarOrdenado e (x:xs)= recr (\x xs res -> if e < x then e : x : res else if null xs then x : e : res else x : res) [] (x:xs)


insertarOrdenado2 :: (Ord a) => a -> [a] -> [a]
insertarOrdenado2 e [] = [e]
insertarOrdenado2 e (x:xs)
        | e <= x = e : x : xs
        | null xs = x : e : xs
        | otherwise = x : insertarOrdenado2 e xs


-- ????????
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a,b)]
armarPares = zip

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares xs ys)
-- ???????

data Nat = Zero | Succ Nat

foldNat :: (Nat -> b -> b) -> b -> [Nat] -> b
foldNat f z [] = z
foldNat f z (x:xs) = f x (foldr f z xs)

data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)
    deriving Show

foldAb :: (a -> b) -> (b -> a -> b -> b) -> Arbol a -> b
foldAb fHoja fNodo (Hoja a) = fHoja a
foldAb fHoja fNodo (Nodo r i d) =
    fNodo (foldAb fHoja fNodo i) r (foldAb fHoja fNodo d)

aej :: Arbol Int
aej = Nodo 0 (Hoja 4)
             (Nodo 9 (Hoja 5) (Hoja 7))

espejo :: Arbol a -> Arbol a
espejo = foldAb Hoja (\resi r resd -> Nodo r resd resi)


ramas :: Arbol a -> [[a]]
ramas = foldAb
    (\a -> [[a]])
    (\i r d -> map (r :) i ++ map (r :) d)

insertarAbb :: (Ord a) => a -> Arbol a -> Arbol a
insertarAbb x (Hoja a) = Nodo x (Hoja a) (Hoja a)
insertarAbb x (Nodo r i d) =
    if(x < r)
        then Nodo r (insertarAbb x i) d
        else Nodo r i (insertarAbb x d)



