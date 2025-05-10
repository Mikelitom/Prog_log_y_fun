cuadrados :: Num int => [int] -> [int]
cuadrados = map (\x -> x * x)

main :: IO ()
main = do
    let entrada = [1, 2, 3, 4, 5]
    let salida = cuadrados entrada
    print salida
