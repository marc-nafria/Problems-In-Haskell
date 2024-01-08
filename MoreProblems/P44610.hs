import Data.List
msort xs = sort (map sort xs)

zerosNones1 :: Int -> [[Int]]

zerosNones1 0 = [[]]
zerosNones1 n = map (0:) xs ++ map (1:) xs
    where xs = zerosNones1 $ n - 1

-- En aquets cas ens donen el nombre de uns que hi ha de haver
zerosNones2 :: Int -> Int -> [[Int]]
zerosNones2 n u
    | n == 0 = [[]]               -- Caso base: Si n es 0, la única combinación es una lista vacía
    | u == 0 = [replicate n 0]    -- Caso base: Si u es 0, la única combinación es una lista con n ceros
    | n == u = [replicate n 1]    -- Caso base: Si n es igual a u, la única combinación es una lista con n unos
    | otherwise = map (0:) (zerosNones2 (n - 1) u) ++ map (1:) (zerosNones2 (n - 1) (u - 1))


subsets1 :: [a] -> [[a]]
subsets1 [] = [[]]  -- Caso base: El subconjunto vacío es el único subconjunto de una lista vacía
subsets1 (x:xs) = subsetsWithoutX ++ map (x:) subsetsWithoutX
    where subsetsWithoutX = subsets1 xs

subsets2 :: Int -> [a] -> [[a]]
subsets2 m l = filter (\x -> length x == m) xs
    where xs = subsets1 l