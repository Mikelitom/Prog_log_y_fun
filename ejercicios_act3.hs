-- Ejercicios implementando diversas funciones en Haskell

-------------------------------------------------------------------------------
-- FUNCIONES BÁSICAS
-------------------------------------------------------------------------------

-- map: Aplica una función a cada elemento de una lista
-- Ejercicio 1: Duplicar cada número en una lista
duplicarNumeros :: [Int] -> [Int]
duplicarNumeros list = map (*2) list

-- Ejercicio 2: Convertir una lista de números a sus cuadrados
cuadrados :: [Int] -> [Int]
cuadrados list = map (\x -> x * x) list

-- filter: Filtra elementos de una lista según un parámetro
-- Ejercicio 3: Filtrar números pares de una lista
numerosPares :: [Int] -> [Int]
numerosPares xs = filter even xs

-- Ejercicio 4: Filtrar palabras que empiezan con una vocal
empiezaConVocal :: [String] -> [String]
empiezaConVocal xs = filter (\x -> not (null x) && elem (head (map toLower x)) "aeiou") xs
  where toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
                 | otherwise = c

-- foldr y foldl: Reducen una lista a un solo valor aplicando una función acumulativa
-- Ejercicio 5: Calcular el producto de todos los elementos usando foldr
productoFoldr :: [Int] -> Int
productoFoldr xs = foldr (*) 1 xs

-- Ejercicio 6: Calcular la suma de todos los elementos usando foldl
sumaFoldl :: [Int] -> Int
sumaFoldl xs = foldl (+) 0 xs

-- (.) Composición de funciones
-- Ejercicio 7: Crear una función que primero incremente en 1 y luego calcule el cuadrado
incrementarYCuadrado :: Int -> Int
incrementarYCuadrado = (^2) . (+1)

-- Ejercicio 8: Crear una función que primero filtre números pares y luego los duplique
filtrarYDuplicar :: [Int] -> [Int]
filtrarYDuplicar = map (*2) . filter even

-------------------------------------------------------------------------------
-- FUNCIONES DE ALTO NIVEL
-------------------------------------------------------------------------------

-- zip y zipWith: Combina listas elemento a elemento
-- Ejercicio 9: Combinar dos listas en pares
combinarListas :: [a] -> [b] -> [(a, b)]
combinarListas xs ys = zip xs ys

-- Ejercicio 10: Sumar elementos correspondientes de dos listas
sumarListas :: [Int] -> [Int] -> [Int]
sumarListas xs ys = zipWith (+) xs ys

-- take, drop: Toma o descarta los primeros elementos de una lista
-- Ejercicio 11: Obtener los primeros N elementos de una lista
primeros :: Int -> [a] -> [a]
primeros n xs = take n xs

-- Ejercicio 12: Descartar los primeros N elementos de una lista
descartar :: Int -> [a] -> [a]
descartar n xs = drop n xs

-- concat y concatMap: Une listas o mapea y concatena
-- Ejercicio 13: Unir una lista de listas en una sola lista
unirListas :: [[a]] -> [a]
unirListas xss = concat xss

-- Ejercicio 14: Generar y unir los múltiplos de cada número
multiplosYConcatena :: [Int] -> [Int]
multiplosYConcatena xs = concatMap (\x -> [x, x*2, x*3]) xs

-------------------------------------------------------------------------------
-- OPERACIONES CON LISTAS
-------------------------------------------------------------------------------

-- elem y notElem: Verifica si un elemento está en una lista
-- Ejercicio 15: Verificar si un número está en la lista
estaEnLista :: Int -> [Int] -> Bool
estaEnLista x xs = elem x xs

-- Ejercicio 16: Verificar si un número no está en la lista
noEstaEnLista :: Int -> [Int] -> Bool
noEstaEnLista x xs = notElem x xs

-- length, reverse, head, last, init, tail, null
-- Ejercicio 17: Calcular la longitud de una lista
longitud :: [a] -> Int
longitud xs = length xs

-- Ejercicio 18: Invertir una lista
invertir :: [a] -> [a]
invertir xs = reverse xs

-- Ejercicio 19: Obtener el primer elemento de una lista
primerElemento :: [a] -> a
primerElemento xs = head xs

-- Ejercicio 20: Obtener el último elemento de una lista
ultimoElemento :: [a] -> a
ultimoElemento xs = last xs

-- Ejercicio 21: Obtener todos los elementos excepto el último
todosExceptoUltimo :: [a] -> [a]
todosExceptoUltimo xs = init xs

-- Ejercicio 22: Obtener todos los elementos excepto el primero
todosExceptoPrimero :: [a] -> [a]
todosExceptoPrimero xs = tail xs

-- Ejercicio 23: Verificar si una lista está vacía
estaVacia :: [a] -> Bool
estaVacia xs = null xs

-- takeWhile, dropWhile: Operan sobre listas basadas en un parámetro
-- Ejercicio 24: Tomar elementos mientras sean menores a un valor
tomarMenoresQue :: Int -> [Int] -> [Int]
tomarMenoresQue n xs = takeWhile (<n) xs

-- Ejercicio 25: Descartar elementos mientras sean menores a un valor
descartarMenoresQue :: Int -> [Int] -> [Int]
descartarMenoresQue n xs = dropWhile (<n) xs

-------------------------------------------------------------------------------
-- FUNCIONES MATEMÁTICAS
-------------------------------------------------------------------------------

-- mod, div, gcd: Para operaciones aritméticas específicas
-- Ejercicio 26: Determinar si un número es divisible por otro
esDivisible :: Int -> Int -> Bool
esDivisible x y = mod x y == 0

-- Ejercicio 27: Dividir un número entre otro usando división entera
divisionEntera :: Int -> Int -> Int
divisionEntera x y = div x y

-- Ejercicio 28: Calcular el máximo común divisor de dos números
maximoComunDivisor :: Int -> Int -> Int
maximoComunDivisor x y = gcd x y

-------------------------------------------------------------------------------
-- FUNCIONES RELACIONADAS CON TIPOS
-------------------------------------------------------------------------------

-- show, read: Conversión entre cadenas y valores
-- Ejercicio 29: Convertir un número a cadena
numeroACadena :: Int -> String
numeroACadena x = show x

-- Ejercicio 30: Convertir una cadena a número
cadenaANumero :: String -> Int
cadenaANumero s = read s

-- type y data: Construcción y manipulación de tipos personalizados
-- Ejercicio 31: Definir un tipo alias para representar coordenadas
type Coordenada = (Int, Int)

distancia :: Coordenada -> Coordenada -> Float
distancia (x1, y1) (x2, y2) = sqrt (fromIntegral ((x2 - x1)^2 + (y2 - y1)^2))

-- Ejercicio 32: Definir un tipo de datos para representar figuras geométricas
data Figura = Circulo Float | Rectangulo Float Float | Triangulo Float Float Float

area :: Figura -> Float
area (Circulo r) = pi * r * r
area (Rectangulo b h) = b * h
area (Triangulo a b c) = 
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))  -- Fórmula de Herón