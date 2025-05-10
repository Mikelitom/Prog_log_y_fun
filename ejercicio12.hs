-- Ejercicio 12: Contar números mayores a 10 en una lista
contarMayoresA10 :: [Int] -> Int
contarMayoresA10 numeros = length (filter (> 10) numeros)

main :: IO ()
main = do
    print $ contarMayoresA10 [5, 12, 18, 7] -- Resultado: 2