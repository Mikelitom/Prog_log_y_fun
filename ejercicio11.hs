
-- Ejercicio 11: Filtrar nombres que empiezan con una letra específica
filtrarNombres :: [String] -> Char -> [String]
filtrarNombres nombres letra = filter (\n -> head n == letra) nombres

main :: IO ()
main = do
    print $ filtrarNombres ["Ana", "Pedro", "Alberto", "Pablo"] 'A' -- Resultado: ["Ana", "Alberto"]