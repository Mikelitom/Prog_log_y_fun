-- Ejercicio 16: Concatenar una lista de strings en una sola cadena
concatenarStrings :: [String] -> String
concatenarStrings palabras = unwords palabras

main :: IO ()
main = do
    print $ concatenarStrings ["Hola", "mundo", "programación", "funcional"] -- Resultado: "Hola mundo programación funcional"