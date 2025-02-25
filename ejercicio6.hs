add :: Int -> (Int -> Int)
add x = \y -> x + y

main :: IO () 
main = do
  putStrLn "Num 1: "
  input1 <- getLine
  let num1 = read input1 :: Int
  
  putStrLn "Num 2: "
  input2 <- getLine
  let num2 = read input2 :: Int

  let result = add num1 num2
  print result
  