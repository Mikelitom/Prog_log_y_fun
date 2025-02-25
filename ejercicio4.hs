squareOddNumbers :: Integral x => [x] -> [x]
squareOddNumbers = map (\x -> x * x) . filter odd

main :: IO ()
main = do
  let input = [1, 2, 3, 4, 5, 6]
  let output = squareOddNumbers input
  print output