-- Setup for some of the exercises
data List a = Cons a (List a)
            | Nil
              deriving (Show)
-- Some code from the chapter that I used so I can test the following exercise
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- Exercises - Page 60
-- Exercise 1
toList :: List a -> [a]
toList (Cons a  as) = [a] ++ toList as
toList Nil = []

-- Exercise 2
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
