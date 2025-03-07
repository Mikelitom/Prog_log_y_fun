-- Ejercicio 19: Encontrar la palabra más larga en una lista
palabraMasLarga :: [String] -> String
palabraMasLarga palabras = foldl1 (\a b -> if length a > length b then a else b) palabras

main :: IO ()
main = do
    print $ palabraMasLarga ["manzana", "pera", "sandía"] -- Resultado: "manzana"