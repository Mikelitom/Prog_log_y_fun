{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use sum" #-}
import Data.Char (isAsciiUpper)
-- Ejercicios implementando diversas funciones en Haskell

-------------------------------------------------------------------------------
-- FUNCIONES BÁSICAS
-------------------------------------------------------------------------------

-- map: Aplica una función a cada elemento de una lista
-- Ejercicio 1: Duplicar cada número en una lista
duplicarNumeros :: [Int] -> [Int]
duplicarNumeros = map (*2)

-- Ejercicio 2: Convertir una lista de números a sus cuadrados
cuadrados :: [Int] -> [Int]
cuadrados = map (\x -> x * x)

-- filter: Filtra elementos de una lista según un parámetro
-- Ejercicio 3: Filtrar números pares de una lista
numerosPares :: [Int] -> [Int]
numerosPares= filter even

-- Ejercicio 4: Filtrar palabras que empiezan con una consonante
empiezaConConsonante :: [String] -> [String]
empiezaConConsonante = filter (\x -> not (null x) && notElem (head (map toLower x)) "aeiou")
  where toLower c | isAsciiUpper c = toEnum (fromEnum c + 32)
                 | otherwise = c

-- foldr y foldl: Reducen una lista a un solo valor aplicando una función acumulativa
-- Ejercicio 5: Calcular el producto de todos los elementos usando foldr
productoFoldr :: [Int] -> Int
productoFoldr = foldr (*) 1

-- Ejercicio 6: Calcular la suma de todos los elementos usando foldl
sumaFoldl :: [Int] -> Int
sumaFoldl = foldl (+) 0

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
combinarListas = zip

-- Ejercicio 10: Sumar elementos correspondientes de dos listas
sumarListas :: [Int] -> [Int] -> [Int]
sumarListas = zipWith (+)

-- take, drop: Toma o descarta los primeros elementos de una lista
-- Ejercicio 11: Obtener los primeros N elementos de una lista
primeros :: Int -> [a] -> [a]
primeros = take

-- Ejercicio 12: Descartar los primeros N elementos de una lista
descartar :: Int -> [a] -> [a]
descartar = drop

-- concat y concatMap: Une listas o mapea y concatena
-- Ejercicio 13: Unir una lista de listas en una sola lista
unirListas :: [[a]] -> [a]
unirListas = concat

-- Ejercicio 14: Generar y unir los múltiplos de cada número
multiplosYConcatena :: [Int] -> [Int]
multiplosYConcatena = concatMap (\x -> [x, x*2, x*3])

-------------------------------------------------------------------------------
-- OPERACIONES CON LISTAS
-------------------------------------------------------------------------------

-- elem y notElem: Verifica si un elemento está en una lista
-- Ejercicio 15: Verificar si un número está en la lista
estaEnLista :: Int -> [Int] -> Bool
estaEnLista = elem

-- Ejercicio 16: Verificar si un número no está en la lista
noEstaEnLista :: Int -> [Int] -> Bool
noEstaEnLista = notElem
-- length, reverse, head, last, init, tail, null
-- Ejercicio 17: Calcular la longitud de una lista
longitud :: [a] -> Int
longitud = length

-- Ejercicio 18: Invertir una lista
invertir :: [a] -> [a]
invertir = reverse

-- Ejercicio 19: Obtener el primer elemento de una lista
primerElemento :: [a] -> a
primerElemento = head

-- Ejercicio 20: Obtener el último elemento de una lista
ultimoElemento :: [a] -> a
ultimoElemento = last

-- Ejercicio 21: Obtener todos los elementos excepto el último
todosExceptoUltimo :: [a] -> [a]
todosExceptoUltimo = init

-- Ejercicio 22: Obtener todos los elementos excepto el primero
todosExceptoPrimero :: [a] -> [a]
todosExceptoPrimero = tail

-- Ejercicio 23: Verificar si una lista está vacía
estaVacia :: [a] -> Bool
estaVacia = null

-- takeWhile, dropWhile: Operan sobre listas basadas en un parámetro
-- Ejercicio 24: Tomar elementos mientras sean menores a un valor
tomarMenoresQue :: Int -> [Int] -> [Int]
tomarMenoresQue n = takeWhile (<n)

-- Ejercicio 25: Descartar elementos mientras sean menores a un valor
descartarMenoresQue :: Int -> [Int] -> [Int]
descartarMenoresQue n = dropWhile (<n)

-------------------------------------------------------------------------------
-- FUNCIONES MATEMÁTICAS
-------------------------------------------------------------------------------

-- mod, div, gcd: Para operaciones aritméticas específicas
-- Ejercicio 26: Determinar si un número es divisible por otro
esDivisible :: Int -> Int -> Bool
esDivisible x y = mod x y == 0

-- Ejercicio 27: Dividir un número entre otro usando división entera
divisionEntera :: Int -> Int -> Int
divisionEntera = div

-- Ejercicio 28: Calcular el máximo común divisor de dos números
maximoComunDivisor :: Int -> Int -> Int
maximoComunDivisor = gcd

-------------------------------------------------------------------------------
-- FUNCIONES RELACIONADAS CON TIPOS
-------------------------------------------------------------------------------

-- show, read: Conversión entre cadenas y valores
-- Ejercicio 29: Convertir un número a cadena
numeroACadena :: Int -> String
numeroACadena = show

-- Ejercicio 30: Convertir una cadena a número
cadenaANumero :: String -> Int
cadenaANumero = read

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

main :: IO ()
main = do
    print "Ejercicio 1. Map (Duplicar cada numero de una lista)"
    print "Entrada: [1,2,3,4,5]"
    let entrada1 = [1,2,3,4,5]
    let salida1 = duplicarNumeros entrada1
    print ("Salida: " ++ show salida1)
    print ""

    print "Ejercicio 2. Map (Cuadrado de cada numero de una lista)"
    print "Entrada: [1,2,3,4,5]"
    let entrada2 = [1,2,3,4,5]
    let salida2 = cuadrados entrada2
    print ("Salida: " ++ show salida2)
    print ""

    print "Ejercicio 3. Filter (Filtrar numero pares)"
    print "Entrada: [1,2,3,4,5]"
    let entrada3 = [1,2,3,4,5]
    let salida3 = numerosPares entrada3
    print ("Salida: " ++ show salida3)
    print ""

    print "Ejercicio 4. Filter (Filtrar palabras que inician en consonante)"
    print "Entreda: [Agua, Pez, Lupe]"
    let entrada4 = ["Agua", "Pez", "Lupe"]
    let salida4 = empiezaConConsonante entrada4
    print ("Salida: " ++ show salida4)
    print ""

    -- Ejercicio 5
    print "Ejercicio 5. Calcular el producto de todos los elementos usando foldr"
    print "Entrada: [1,2,3,4,5]"
    let entrada5 = [1,2,3,4,5]
    let salida5 = productoFoldr entrada5
    print ("Salida: " ++ show salida5)
    print ""

    -- Ejercicio 6
    print "Ejercicio 6. Calcular la suma de todos los elementos usando foldl"
    print "Entrada: [1,2,3,4,5]"
    let entrada6 = [1,2,3,4,5]
    let salida6 = sumaFoldl entrada6
    print ("Salida: " ++ show salida6)
    print ""

    -- Ejercicio 7
    print "Ejercicio 7. Incrementar en 1 y luego calcular el cuadrado"
    print "Entrada: 4"
    let entrada7 = 4
    let salida7 = incrementarYCuadrado entrada7
    print ("Salida: " ++ show salida7)
    print ""

    -- Ejercicio 8
    print "Ejercicio 8. Filtrar números pares y luego duplicarlos"
    print "Entrada: [1,2,3,4,5,6]"
    let entrada8 = [1,2,3,4,5,6]
    let salida8 = filtrarYDuplicar entrada8
    print ("Salida: " ++ show salida8)
    print ""

    -- Ejercicio 9
    print "Ejercicio 9. Combinar dos listas en pares"
    print "Entrada: [1,2,3], ['a','b','c']"
    let entrada9a = [1,2,3]
    let entrada9b = ['a','b','c']
    let salida9 = combinarListas entrada9a entrada9b
    print ("Salida: " ++ show salida9)
    print ""

    -- Ejercicio 10
    print "Ejercicio 10. Sumar elementos correspondientes de dos listas"
    print "Entrada: [1,2,3], [4,5,6]"
    let entrada10a = [1,2,3]
    let entrada10b = [4,5,6]
    let salida10 = sumarListas entrada10a entrada10b
    print ("Salida: " ++ show salida10)
    print ""

    -- Ejercicio 11
    print "Ejercicio 11. Obtener los primeros N elementos de una lista"
    print "Entrada: 3, [1,2,3,4,5]"
    let salida11 = primeros 3 [1,2,3,4,5]
    print ("Salida: " ++ show salida11)
    print ""

    -- Ejercicio 12
    print "Ejercicio 12. Descartar los primeros N elementos de una lista"
    print "Entrada: 2, [1,2,3,4,5]"
    let salida12 = descartar 2 [1,2,3,4,5]
    print ("Salida: " ++ show salida12)
    print ""

    -- Ejercicio 13
    print "Ejercicio 13. Unir una lista de listas en una sola lista"
    print "Entrada: [[1,2], [3,4], [5]]"
    let salida13 = unirListas [[1,2], [3,4], [5]]
    print ("Salida: " ++ show salida13)
    print ""

    -- Ejercicio 14
    print "Ejercicio 14. Generar y unir los múltiplos de cada número"
    print "Entrada: [1,2,3]"
    let salida14 = multiplosYConcatena [1,2,3]
    print ("Salida: " ++ show salida14)
    print ""

    -- Ejercicio 15
    print "Ejercicio 15. Verificar si un número está en la lista"
    print "Entrada: 3, [1,2,3,4,5]"
    let salida15 = estaEnLista 3 [1,2,3,4,5]
    print ("Salida: " ++ show salida15)
    print ""

    -- Ejercicio 16
    print "Ejercicio 16. Verificar si un número no está en la lista"
    print "Entrada: 6, [1,2,3,4,5]"
    let salida16 = noEstaEnLista 6 [1,2,3,4,5]
    print ("Salida: " ++ show salida16)
    print ""

    -- Ejercicio 17
    print "Ejercicio 17. Calcular la longitud de una lista"
    print "Entrada: [1,2,3,4,5]"
    let salida17 = longitud [1,2,3,4,5]
    print ("Salida: " ++ show salida17)
    print ""

    -- Ejercicio 18
    print "Ejercicio 18. Invertir una lista"
    print "Entrada: [1,2,3,4,5]"
    let salida18 = invertir [1,2,3,4,5]
    print ("Salida: " ++ show salida18)
    print ""

    -- Ejercicio 19
    print "Ejercicio 19. Obtener el primer elemento de una lista"
    print "Entrada: [1,2,3,4,5]"
    let salida19 = primerElemento [1,2,3,4,5]
    print ("Salida: " ++ show salida19)
    print ""

    -- Ejercicio 20
    print "Ejercicio 20. Obtener el último elemento de una lista"
    print "Entrada: [1,2,3,4,5]"
    let salida20 = ultimoElemento [1,2,3,4,5]
    print ("Salida: " ++ show salida20)
    print ""

    -- Ejercicio 21
    print "Ejercicio 21. Obtener todos los elementos excepto el último"
    print "Entrada: [1,2,3,4,5]"
    let salida21 = todosExceptoUltimo [1,2,3,4,5]
    print ("Salida: " ++ show salida21)
    print ""

    -- Ejercicio 22
    print "Ejercicio 22. Obtener todos los elementos excepto el primero"
    print "Entrada: [1,2,3,4,5]"
    let salida22 = todosExceptoPrimero [1,2,3,4,5]
    print ("Salida: " ++ show salida22)
    print ""

    -- Ejercicio 23
    print "Ejercicio 23. Verificar si una lista está vacía"
    print "Entrada: []"
    let salida23 = estaVacia []
    print ("Salida: " ++ show salida23)
    print ""

    -- Ejercicio 24
    print "Ejercicio 24. Tomar elementos mientras sean menores a un valor"
    print "Entrada: 4, [1,2,3,4,5]"
    let salida24 = tomarMenoresQue 4 [1,2,3,4,5]
    print ("Salida: " ++ show salida24)
    print ""

    -- Ejercicio 25
    print "Ejercicio 25. Descartar elementos mientras sean menores a un valor"
    print "Entrada: 4, [1,2,3,4,5]"
    let salida25 = descartarMenoresQue 4 [1,2,3,4,5]
    print ("Salida: " ++ show salida25)
    print ""

    -- Ejercicio 26
    print "Ejercicio 26. Determinar si un número es divisible por otro"
    print "Entrada: 10, 5"
    let salida26 = esDivisible 10 5
    print ("Salida: " ++ show salida26)
    print ""

    -- Ejercicio 27
    print "Ejercicio 27. Dividir un número entre otro usando división entera"
    print "Entrada: 10, 3"
    let salida27 = divisionEntera 10 3
    print ("Salida: " ++ show salida27)
    print ""

    -- Ejercicio 28
    print "Ejercicio 28. Calcular el máximo común divisor de dos números"
    print "Entrada: 24, 36"
    let salida28 = maximoComunDivisor 24 36
    print ("Salida: " ++ show salida28)
    print ""
