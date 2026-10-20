-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre:
--  Carnet:
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
limpiarTexto (x:xs) = 
  if isAlphaNum x then [toLower x] ++ limpiarTexto xs
  else limpiarTexto xs

invertirTexto :: String -> String
invertirTexto [] = []
invertirTexto (x:xs) = (invertirTexto xs) ++ [x]

esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo a = b == invertirTexto b
  where b = limpiarTexto a
    
-- (x:xs) = 
--    if isAlphaNum x then c = [toLower x]
--  else "Pa"

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- Uso una función auxiliar con parámetros acumulativos para arrastrar los valores. Así recorro la 
-- lista una sola vez: en cada paso, si el elemento actual es mayor al máximo o menor al mínimo, 
-- los paso actualizados a la siguiente llamada. Al vaciarse la lista, la tupla ya tiene el resultado final.
aux :: Ord a => [a] -> a -> a -> (a, a)
aux [] max min = (max, min)
aux (x:xs) max min =
  if x > max then (aux xs x min)
  else if x < min then (aux xs max x)
  else (aux xs max min)

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (aux xs x x)

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora = undefined

duracion :: String -> String -> Either String Int
duracion = undefined

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
