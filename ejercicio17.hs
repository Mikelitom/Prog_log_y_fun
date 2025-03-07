-- Ejercicio 17: Filtrar palabras que contienen una letra específica
filtrarPalabrasConLetra :: [String] -> Char -> [String]
filtrarPalabrasConLetra palabras letra = filter (elem letra) palabras

main :: IO ()
main = do
    print $ filtrarPalabrasConLetra ["gato", "perro", "elefante", "loro"] 'o' -- Resultado: ["gato", "loro"]