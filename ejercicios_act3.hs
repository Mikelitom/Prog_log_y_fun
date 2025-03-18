-- Ejercicio 1: Funcion Map
funcionMap :: Num [a] => [a]
funcionMap lista = map (\x -> x * 3) lista

main :: IO ()
main = do
    print "Ejercicio 1: Map"
    print "Triple de cada numero -> [1,2,3,4,5] -> [1,6,9,12,15]"
    let entradaMap = [1,2,3,4,5]
    let salidaMap = funcionMap entradaMap
    print salidaMap