fizzBuzz :: [Either Int String]
fizzBuzz = fizzBuzz' 0
  where 
    fizzBuzz' :: Int -> [Either Int String]
    fizzBuzz' n
      | (mod n 3 == 0) && (mod n 5 == 0) = Right "FizzBuzz" : fizzBuzz' (n+1)
      | (mod n 3 == 0) = Right "Fizz" : fizzBuzz' (n+1)
      | (mod n 5 == 0) = Right "Buzz" : fizzBuzz' (n+1)
      | otherwise = Left n : fizzBuzz' (n+1)