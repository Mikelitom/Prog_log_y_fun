-- Ejercicio 20: Contar cuántas veces aparece una letra en una lista de palabras
contarLetraEnPalabras :: [String] -> Char -> Int
contarLetraEnPalabras palabras letra = sum (map (length . filter (== letra)) palabras)

main :: IO ()
main = do
    print $ contarLetraEnPalabras ["banana", "manzana", "naranja"] 'a' -- Resultado: 7