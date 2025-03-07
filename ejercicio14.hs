-- Ejercicio 14: Filtrar números negativos
filtrarNegativos :: [Int] -> [Int]
filtrarNegativos numeros = filter (>= 0) numeros

main :: IO ()
main = do
    print $ filtrarNegativos [3, -1, 5, -7, 8, -2] -- Resultado: [3, 5, 8]