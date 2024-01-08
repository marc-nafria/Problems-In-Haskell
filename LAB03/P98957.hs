-- Generate the sequence of ones [1,1,1,1,1,1,1,1,…].
ones :: [Integer]
ones = 1 : ones

-- Generate the sequence of the natural numbers [0,1,2,3,4,5,6,7…].
nats :: [Integer]
nats = 0 : map (+ 1) nats

-- Generate the sequence of the integer numbers [0,1,−1,2,−2,3,−3,4…].
ints' :: Integer -> Integer
ints' x
  | x > 0 = -x
  | x <= 0 = -x + 1

ints :: [Integer]
ints = 0 : map ints' ints

-- Generate the sequence of the triangular numbers: 0,1,3,6,10,15,21,28,…].
triangulars :: [Integer]
triangulars = scanl (+) 0 $ tail nats

-- Generate the sequence of the factorial numbers: [1,1,2,6,24,120,720,5040,…].
factorials :: [Integer]
factorials = scanl (*) 1 $ tail nats

-- Generate the sequence of the Fibonacci numbers: [0,1,1,2,3,5,8,13,…].
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Generate the sequence of prime numbers: [2,3,5,7,11,13,17,19,…].
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = isPrime' 2
  where
    isPrime' :: Integer -> Bool
    isPrime' d
      | d == n = True
      | mod n d == 0 = False
      | otherwise = isPrime' (d + 1)

primes :: [Integer]
primes = filter isPrime (drop 2 nats)

-- Generate the ordered sequence of the Hamming numbers: [1,2,3,4,5,6,8,9,…]. The Hamming numbers are those that only have 2, 3 and 5 as prime divisors.
{-
merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

msort :: [Integer] -> [Integer]
msort [] = []
msort [x] = [x]
msort a = merge (msort $ take d a) (msort $ drop d a)
  where
    n = length a
    d = div n 2

hamming :: [Integer]
hamming = 1 : msort (msort (map (* 2) hamming) (map (* 3) hamming)) (map (* 5) hamming)
-}

-- Generate the look-and-say sequence: [1,11,21,1211,111221,312211,13112221,1113213211,…].
-- Generate the sequences of rows of the Tartaglia triangle (also known as Pascal’s triangle): [[1],[1,1],[1,2,1],[1,3,3,1],…].