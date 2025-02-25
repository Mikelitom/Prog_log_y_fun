evenNumbers :: Integral x => [x] -> [x]
evenNumbers = filter even

main :: IO ()
main = do
  let entrada = [1, 2, 3, 4, 5, 6]
  let salida = evenNumbers entrada
  print salida