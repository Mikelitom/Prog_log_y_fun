-- Ejercicio 15: Obtener la primera letra de cada palabra
primeraLetra :: [String] -> [Char]
primeraLetra palabras = map head palabras

main :: IO ()
main = do
    print $ primeraLetra ["manzana", "pera", "kiwi"] -- Resultado: ['m', 'p', 'k']