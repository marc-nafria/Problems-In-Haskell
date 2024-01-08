-- 1. Implement a function flatten :: [[Int]] -> [Int] that flattens a list of lists of integers in a list of integers.
flatten :: [[Int]] -> [Int]
flatten = foldr (++) []

-- 2. Implement a function myLength :: String -> Int that returns the length of a string.
myLength :: String -> Int
myLength s = foldr (+) 0 $ map (const 1) s

-- 3. Implement a function myReverse :: [Int] -> [Int] that reverses a list of integers.
myReverse :: [Int] -> [Int]
myReverse = foldl (flip (:)) []

-- 4. Implement a function countIn :: [[Int]] -> Int -> [Int] that, given a list of sublists ℓ and an element x, returns the list that tells who many times x appears in each sublist of ℓ.

countIn :: [[Int]] -> Int -> [Int]
countIn a x = map (length . filter (== x)) a

-- 5. Implement a function firstWord :: String -> String that, given a string with blanks and alphabetic characters, returns its first word
firstWord :: String -> String
firstWord = takeWhile (/= ' ') . dropWhile (== ' ')