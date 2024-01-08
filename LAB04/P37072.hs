data Tree a = Node a (Tree a) (Tree a) | Empty 
  deriving (Show)

-- Write a function size :: Tree a -> Int that, given a tree, returns its size,
-- that is, the number of node it contains.
size :: Tree a -> Int
size Empty = 0
size (Node t esq drt) = 1 + (size esq) + (size drt)

-- Write a function height :: Tree a -> Int that, given a tree, returns its height,
-- assuming that empty trees have zero height.
height :: Tree a -> Int
height Empty = 0
height (Node t esq drt) = 1 + max (height esq) (height drt)

-- Write a function equal :: Eq a => Tree a -> Tree a -> Bool that, given two trees,
-- tells whether they are the same.

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node t2 e2 d2) = False
equal (Node t1 e1 d1) Empty = False
equal (Node t1 e1 d1) (Node t2 e2 d2) =  ((t1 == t2) && (equal e1 e2) && (equal d1  d2))

-- Write a function isomorphic :: Eq a => Tree a -> Tree a -> Bool that, given two
-- trees, tells whether they are isomorphic, that is, if one can obtain one from the
-- other flipping some of its descendants.
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty (Node t2 e2 d2) = False
isomorphic (Node t1 e1 d1) Empty = False
isomorphic (Node t1 e1 d1) (Node t2 e2 d2) = (t1 == t2) && (((equal e1 e2) && (equal d1  d2)) || ((equal e1 d2) && (equal d1 e2)))

-- Write a function preOrder :: Tree a -> [a] that, given a tree, return its pre-order
-- traversal.
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node t e d) = [t] ++ (preOrder e) ++ (preOrder d)

-- Write a function postOrder :: Tree a -> [a] that, given a tree, return its post-order traversal.
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node t e d) = (postOrder e) ++ (postOrder d) ++ [t]

-- Write a function inOrder :: Tree a -> [a] that, given a tree, return its in-order traversal.
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node t e d) = (inOrder e) ++ [t] ++ (inOrder d)

-- Write a function breadthFirst :: Tree a -> [a] that, given a tree, return its traversal
-- by levels.
-- *****CHAT_GPT*****
breadthFirst :: Tree a -> [a]
breadthFirst tree = bfs [tree]
  where
    bfs [] = []
    bfs xs = map nodeValue xs ++ bfs (concatMap children xs)
    
    nodeValue Empty          = []
    nodeValue (Node a _ _)   = [a]

    children Empty           = []
    children (Node _ left right) = [left, right]

-- Write a function build :: Eq a => [a] -> [a] -> Tree a that, given a pre-order traversal
-- of a tree and an in-order traversal of the same tree, returns the original tree. You can
-- assume that the three has no repeated elements.

-- Write a function overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a that, given two trees,
-- returns its overlapping using a function. Overlapping two trees with a function consists
-- in placing the two trees one on the other and combine the double nodes using the given function.

-- Arbre per provar:
t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
{-
                  t1
              /        \
            t2          t3
          /   \        /   \
        t4     t5     t6    t7
-}