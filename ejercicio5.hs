factorial :: (Eq x, Num x) => x -> x
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do 
  let input = 5
  let output = factorial input
  print output