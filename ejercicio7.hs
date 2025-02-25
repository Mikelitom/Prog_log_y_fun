duplicados :: Num int => [int] -> [int]
duplicados = map (\x -> x + x)

main :: IO ()
main = do
    let entrada = [1, 2, 3, 4, 5]
    let salida = duplicados entrada
    print salida