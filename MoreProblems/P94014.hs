fib :: Int -> Integer
fib n = fibMemo !! n

fibMemo = 0 : 1 : zipWith (+) fibMemo (tail fibMemo)
