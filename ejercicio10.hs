suma :: [Int] -> [Int]
suma = map (+ 1)

main :: IO ()
main = do 
    let numeros = [1, 2, 3, 4, 5]
    let numerosMas1 = suma numeros
    print numerosMas1