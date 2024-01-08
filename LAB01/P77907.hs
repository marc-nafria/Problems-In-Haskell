absValue :: Int -> Int
absValue n
  | n >= 0 = n
  | otherwise = -n -- otherwise significa true (pero aixi es mes llegible)

power :: Int -> Int -> Int
power _ 0 = 1 -- qualsevol nombre elevat a zero es 1
power x n = x * power x (n - 1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isPrime' 2
  where
    isPrime' :: Int -> Bool
    isPrime' d
      | (d == n) = True
      | (mod n d == 0) = False
      | otherwise = isPrime' (d + 1)

slowFib :: Int -> Int
slowFib n = slowFib' n
  where
    slowFib' :: Int -> Int
    slowFib' x
      | (x == 0) = 0
      | (x == 1) = 1
      | otherwise = slowFib' (x - 2) + slowFib' (x - 1)

-- f(0) = 0, f(1) = 1, f(2) = 1+0=1, f(3) = 1+1=2, f(4) = 2 + 1
quickFib :: Int -> Int
quickFib 0 = 0
quickFib 1 = 1
quickFib n = quickFib' [0, 1] (n - 1)
  where
    quickFib' :: [Int] -> Int -> Int
    quickFib' (x : y : _) 1 = (x + y)
    quickFib' (x : y : _) n = quickFib' [y, x + y] (n - 1)
