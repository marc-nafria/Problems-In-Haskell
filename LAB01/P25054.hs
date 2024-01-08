myLength :: [Int] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- Define a function myMaximum :: [Int] -> Int that, given a non-empty list
-- of integers, returns its maximal element.
myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x : xs) = max x (myMaximum xs)

{- Algortime de merda
myMaximum a
    | (myLength a == 1) = head a
    | (head a <= last a) = myMaximum (tail a)  -- treiem el primer
    | otherwise = myMaximum (tail (reverse a))    -- donem la vota i treiem el primer
-}

-- Define a function average :: [Int] -> Float that, given a non-empty list of integers, returns its average.
average :: [Int] -> Float
average [] = 0
average a = fromIntegral (sumList a) / fromIntegral (myLength a)
  where
    sumList :: [Int] -> Int
    sumList [] = 0
    sumList (x : xs) = x + sumList xs

-- Define a function buildPalindrome :: [Int] -> [Int] that, given a list, returns its palindrome that starts with the reserved list.

buildPalindrome :: [Int] -> [Int]
buildPalindrome a = reverse a ++ a

-- Define a function remove :: [Int] -> [Int] -> [Int] that given a list of integers x and a list of integers y, returns x after having removed all the ocurrences of the elements in y.
remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove (x : xs) y
  | elem x y = remove xs y
  | otherwise = x : remove xs y

-- Define a function flatten :: [[Int]] -> [Int] that flattens a list of lists yielding a single list of elements.

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x : xs) = x ++ flatten xs

-- Define a function oddsNevens :: [Int] -> ([Int],[Int]) that, given a list of integers, returns two lists: Onw with all the even numbers and one with all the odd numbers, each of them in the same relative order as in the original list.

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens [] = ([], [])
oddsNevens a = (odds a, evens a)
  where
    odds :: [Int] -> [Int]
    odds [] = []
    odds (x : xs)
      | mod x 2 == 1 = [x] ++ odds xs
      | otherwise = odds xs
    evens :: [Int] -> [Int]
    evens [] = []
    evens (x : xs)
      | mod x 2 == 0 = [x] ++ evens xs
      | otherwise = evens xs

-- Define a function primeDivisors :: Int -> [Int] that returns the list of prime divisors of a non-zero natural.
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isPrime' 2
  where
    isPrime' :: Int -> Bool
    isPrime' d
      | (d == n) = True
      | (mod n d == 0) = False
      | otherwise = isPrime' (d + 1)

primeDivisors :: Int -> [Int]
primeDivisors x = primeDivisors' x [2 .. x]
  where
    primeDivisors' _ [] = []
    primeDivisors' x (y : ys)
      | (isPrime y) && (mod x y == 0) = [y] ++ primeDivisors' x ys
      | otherwise = primeDivisors' x ys
