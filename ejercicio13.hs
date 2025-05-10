-- Ejercicio 13: Multiplicar todos los elementos de una lista por un número
multiplicarPorN :: [Int] -> Int -> [Int]
multiplicarPorN numeros n = map (* n) numeros

main :: IO ()
main = do
    print $ multiplicarPorN [1, 2, 3, 4] 3 -- Resultado: [3, 6, 9, 12]