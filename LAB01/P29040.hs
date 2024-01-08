-- 1. Define a function insert :: [Int] -> Int -> [Int] that, given a sorted list and an element, correctly inserts the new element in the list.

insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert [x] n
  | n < x = [n, x]
  | otherwise = [x, n]
insert (x : xs) n
  | n < x = n : x : xs
  | otherwise = x : insert xs n

-- 2. Define a function isort :: [Int] -> [Int] that implements insertion sort using the previous function.
isort :: [Int] -> [Int]
isort [] = []
isort a = isort' a []
  where
    isort' :: [Int] -> [Int] -> [Int]
    isort' [] y = y
    isort' (x : xs) y = isort' xs (insert y x)

-- 3. Define a function remove :: [Int] -> Int -> [Int] that, given a list and an element x, erases the first occurrence of x from the list. You can assume that the element is always in the list.

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x : xs) y
  | x == y = xs
  | otherwise = x : remove xs y

-- 4. Define a function ssort :: [Int] -> [Int] that implements selection sort using the previous function.

ssort :: [Int] -> [Int]
ssort a = ssort' a []
  where
    ssort' :: [Int] -> [Int] -> [Int]
    ssort' [] y = y
    ssort' xs ys = ssort' (remove xs y) (y : ys)
      where
        y = maximum xs

-- 5. Define a function merge :: [Int] -> [Int] -> [Int] that, given two sorted lists, merges them to get a list with all the elements in sorted order.
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- 6. Define a function msort :: [Int] -> [Int] that implements merge sort using the previous function.
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort a = merge (msort $ take d a) (msort $ drop d a)
  where
    n = length a
    d = div n 2

-- 7. Define a function qsort :: [Int] -> [Int] that implements quick sort.
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x : xs) =
  let petit = qsort $ filter (<= x) xs
      gran = qsort $ filter (> x) xs
   in petit ++ [x] ++ gran

-- 8. Generalize the previous function into genQsort :: Ord a => [a] -> [a] that sorts elements of any type.
genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x : xs) =
  let petit = genQsort $ filter (<= x) xs
      gran = genQsort $ filter (> x) xs
   in petit ++ [x] ++ gran
