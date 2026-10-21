-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Ángel Pachco C.
--  Carnet: 20-10479
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Uso de Gemini Pro para aprender los basics de Haskell y entender errores de sintaxis.
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
limpiarTexto :: String -> String
limpiarTexto [] = []
limpiarTexto (x : xs)
  | isAlphaNum x = [toLower x] ++ limpiarTexto xs
  | otherwise = limpiarTexto xs

invertirTexto :: String -> String
invertirTexto [] = []
invertirTexto (x : xs) = (invertirTexto xs) ++ [x]

esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo a = b == invertirTexto b
  where
    b = limpiarTexto a

-- (x:xs) =
--    if isAlphaNum x then c = [toLower x]
--  else "Pa"

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- Uso una función auxiliar con parámetros acumulativos para arrastrar los valores. Así recorro la
-- lista una sola vez: en cada paso, si el elemento actual es mayor al máximo o menor al mínimo,
-- los paso actualizados a la siguiente llamada. Al vaciarse la lista, la tupla ya tiene el resultado final.

aux :: (Ord a) => [a] -> a -> a -> (a, a)
aux [] maximo minimo = (maximo, minimo)
aux (x : xs) maximo minimo
  | x > maximo = aux xs x minimo
  | x < minimo = aux xs maximo x
  | otherwise = aux xs maximo minimo

mayorYMenor :: (Ord a) => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x : xs) = Just (aux xs x x)

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [h1, h2, ':', m1, m2]
  | not (isDigit h1 && isDigit h2 && isDigit m1 && isDigit m2) = Left "formato inválido"
  | hora > 23 = Left "hora fuera de rango"
  | minutos > 59 = Left "minutos fuera de rango"
  | otherwise = Right (hora, minutos)
  where
    hora = 10 * digitToInt h1 + digitToInt h2
    minutos = 10 * digitToInt m1 + digitToInt m2
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion h1 h2 = case (leerHora h1, leerHora h2) of
  (Right (h1', m1'), Right (h2', m2')) ->
    let min1 = h1' * 60 + m1'
        min2 = h2' * 60 + m2'
     in Right (if min1 <= min2 then min2 - min1 else 1440 - min1 + min2)
  (Left err, _) -> Left err
  (_, Left err) -> Left err

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral x arr =
  if cantFinal > 0.0
    then Just (sumaFinal / cantFinal)
    else Nothing
  where
    (sumaFinal, cantFinal) = foldr (\a (suma, cant) -> if a > x then (suma + a, cant + 1) else (suma, cant)) (0.0, 0.0) arr

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (fila, col) = [(fila + df, col + dc) | df <- [-2, -1, 1, 2], dc <- [-2, -1, 1, 2], fila + df > 0, fila + df < 9, col + dc > 0, col + dc < 9, abs df /= abs dc]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos (fila, col) = length (foldr (\x acum -> if foldr (\y yaEsta -> (y == x) || yaEsta) False acum then acum else x : acum) [] lista)
  where
    lista = [pos2 | pos1 <- movimientosCaballo (fila, col), pos2 <- movimientosCaballo pos1]

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f lista = foldr (\x acc -> f x : acc) [] lista

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter f lista = foldr (\x acc -> if f x then x : acc else acc) [] lista

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f x ys = foldr (\y function a -> function (f a y)) id ys x
