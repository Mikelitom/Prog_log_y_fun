elevarAlCubo :: [Int] -> [Int]
elevarAlCubo = map (^3)

-- Ejemplo de uso
main :: IO ()
main = print (elevarAlCubo [1, 2, 3, 4])  -- Output: [1, 8, 27, 64]