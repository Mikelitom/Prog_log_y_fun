import Data.Char (toUpper)

mayusculas :: [String] -> [String]
mayusculas = map (map toUpper)

main :: IO ()
main = do
    let palabras = ["Miguel", "Fajardo"]
    let palabrasMayuscula = mayusculas palabras
    print palabrasMayuscula