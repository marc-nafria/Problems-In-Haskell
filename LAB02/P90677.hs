-- myFoldl: Left fold function
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- myFoldr: Right fold function
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- myIterate: Generate an infinite list by repeatedly applying a function
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- myUntil: Keep applying a function until a condition is met
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil cond fn x
  | cond x = x
  | otherwise = myUntil cond fn (fn x)

-- myMap: Map function
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- myFilter: Filter function
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs

-- myAll: Check if all elements satisfy a condition
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs) = f x && myAll f xs

-- myAny: Check if any element satisfies a condition
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- myZip: Zipping two lists together
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- myZipWith: Zip using a custom function
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
