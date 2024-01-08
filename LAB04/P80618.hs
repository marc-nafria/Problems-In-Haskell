-- 1. -----------------------------------------------------
data Queue a = Queue [a] [a]
  deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue xs ys) = Queue xs (x : ys)

pop :: Queue a -> Queue a
pop (Queue [] ys) = Queue (tail $ reverse ys) []
pop (Queue (x : xs) ys) = Queue xs ys

top :: Queue a -> a
top (Queue [] [y]) = y
top (Queue [] (y : ys)) = top $ Queue [] ys
top (Queue (x : xs) _) = x
top _ = error "empty queue"

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

-- 2. -----------------------------------------------------

instance Eq a => Eq (Queue a) where
  (Queue x1 y1) == (Queue x2 y2) = (x1 ++ (reverse y1)) == (x2 ++ (reverse y2))
