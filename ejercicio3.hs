reduce :: Num x => [x] -> x
reduce = foldl (+) 0

main :: IO ()
main = do
  let entrada = [1, 2, 3, 4, 5]
  let salida = reduce entrada
  print salida