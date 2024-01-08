-- 1. Implement a function eql :: [Int] -> [Int] -> Bool that tells wether two lists of integers are equal.
eql :: [Int] -> [Int] -> Bool
eql a b
  | length a == length b = and $ zipWith eql' a b
  | otherwise = False
  where
    eql' :: Int -> Int -> Bool
    eql' x y = x == y

-- 2. Implement a function prod :: [Int] -> Int that returns the product of a list of integers.
prod :: [Int] -> Int
prod = foldl (*) 1

-- 3. Implement a function prodOfEvens :: [Int] -> Int that returns the product of all even numbers of a list of integers.
prodOfEvens :: [Int] -> Int
prodOfEvens a = prod $ filter (\x -> mod x 2 == 0) a

-- 4. Implement a function powersOf2 :: [Int] that generates the list of all the powers of 2.
powersOf2 :: [Int]
powersOf2 = iterate (* 2) 1

-- 5. Implement a function scalarProduct :: [Float] -> [Float] -> Float that returns the dot product of two lists of float numbers with the same size.

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct a b = sum $ zipWith (*) a b