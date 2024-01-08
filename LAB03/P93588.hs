-- Implement a function myMap :: (a -> b) -> [a] -> [b] that emulates map using comprehension lists.
myMap :: (a -> b) -> [a] -> [b]
myMap f a = [f x | x <- a]

-- Implement a function myFilter :: (a -> Bool) -> [a] -> [a] that emulates filter using comprehension lists.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond a = [x | x <- a, cond x]

-- Implement a function myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] that emulates zipWith using comprehension lists and zip.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = [f (fst x) (snd x) | x <- c]
  where
    c = zip a b

-- Implement a function thingify :: [Int] -> [Int] -> [(Int, Int)] that, given two lists of integers, returns the list that pairs the elements if the element of the second list divides the one in the first list.
thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify a b = [(x, y) | x <- a, y <- b, mod x y == 0]

-- Implement a function factors :: Int -> [Int] that, given a non-null natural number, generates the ordered list with all its factors (non necessaryly primes)
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], mod n x == 0]
