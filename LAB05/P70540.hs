data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

-- 1. ----------------------------------------------------------

eval1 :: Expr -> Int
eval1 (Val n) = n
eval1 (Add e1 e2) = eval1 e1 + eval1 e2
eval1 (Sub e1 e2) = eval1 e1 - eval1 e2
eval1 (Mul e1 e2) = eval1 e1 * eval1 e2
eval1 (Div e1 e2) = eval1 e1 `div` eval1 e2

-- 2. ----------------------------------------------------------

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Add e1 e2) = do
  x <- eval2 e1
  y <- eval2 e2
  return (x + y)
eval2 (Sub e1 e2) = do
  x <- eval2 e1
  y <- eval2 e2
  return (x - y)
eval2 (Mul e1 e2) = do
  x <- eval2 e1
  y <- eval2 e2
  return (x * y)
eval2 (Div e1 e2) = do
  x <- eval2 e1
  y <- eval2 e2
  if y /= 0 then return (x `div` y) else Nothing

-- 3. ---------------------------------------------------------

eval3 :: Expr -> Either String Int
eval3 (Val n) = Right n
eval3 (Add e1 e2) = do
  x <- eval3 e1
  y <- eval3 e2
  return (x + y)
eval3 (Sub e1 e2) = do
  x <- eval3 e1
  y <- eval3 e2
  return (x - y)
eval3 (Mul e1 e2) = do
  x <- eval3 e1
  y <- eval3 e2
  return (x * y)
eval3 (Div e1 e2) = do
  x <- eval3 e1
  y <- eval3 e2
  if y /= 0 then return (x `div` y) else Left "div0"