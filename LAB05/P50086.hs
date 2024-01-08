-- P80618.hs

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

instance Eq a => Eq (Queue a) where
  (Queue x1 y1) == (Queue x2 y2) = (x1 ++ (reverse y1)) == (x2 ++ (reverse y2))

{-

Es demana el codi de diverses funcions i instanciacions. Seguiu el format dels exemples que es mostren als exemples.

1. Feu que Queue sigui instància de la classe Functor. Per això implementeu la funció fmap que, donada una funció de tipus p -> q i un Queue d’elements de tipus p, retorna un Queue de tipus q resultant d’aplicar la funció a tots els elements de la cua.

2. Feu una funció translation :: Num b => b -> Queue b -> Queue b que aplica una translació a tots els punts d’una cua (que serà el segon paràmetre).

3. Feu que Queue sigui instància de la classe Monad. Per a resoldre aquest apartat, pot ser útil fer una operació que faci la unió de dues cues del mateix tipus.

4. Feu, utilitzant la notació do, una funció kfilter :: (p -> Bool) -> Queue p -> Queue p que selecciona tots els elements d’una cua que satisfan una propietat donada.

-}

-- 1. ----------------------------------------------------
instance Functor Queue where
  fmap f (Queue xs ys) = Queue (map f xs) (map f ys)

-- 2. ----------------------------------------------------
translation :: Num b => b -> Queue b -> Queue b
translation n (Queue xs ys) = Queue (map (+ n) xs) (map (+ n) ys)

-- 3. ----------------------------------------------------
instance Applicative Queue where
  pure x = Queue [x] []
  (Queue fs1 fs2) <*> (Queue xs1 xs2) = Queue (fs1 <*> xs1) (fs2 <*> xs2)

instance Monad Queue where
  return x = Queue [x] []

  (Queue xs1 ys1) >>= f = foldl (\acc x -> acc `concatenate` f x) (Queue [] []) (xs1 ++ reverse ys1)

concatenate :: Queue a -> Queue a -> Queue a
concatenate (Queue xs1 ys1) (Queue xs2 ys2) = Queue xs1 (ys1 ++ reverse xs2 ++ ys2)

-- 4. ----------------------------------------------------
kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter pred queue = do
  x <- queue
  if pred x
    then return x
    else Queue [] []