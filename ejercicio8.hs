longitud :: [String] -> [Int]
longitud = map length

main :: IO ()
main = do
    let palabras = ["Miguel", "Fajardo"]
    let longitudPalabras = longitud palabras
    print longitudPalabras