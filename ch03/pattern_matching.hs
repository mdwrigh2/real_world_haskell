-- Exercises Page 69
import Data.List

-- Exercise 1 & 2
-- 1. Write a function that computes the number of elements in a list. To test it, ensure
--    that it gives the same answers as the standard length function.
-- 2. Add a type signature for your function to your source file. To test it, load the source
--    file into ghci again.

myLength :: [a] -> Integer
myLength (_:xs) = 1 + myLength xs
myLength []     = 0

-- Exercise 3
-- 3. Write a function that computes the mean of a list, i.e., the sum of all elements in
--    the list divided by its length. (You may need to use the fromIntegral function to
--    convert the length of the list from an integer into a floating-point number.)

meanList xs = (sum xs)/fromIntegral(myLength xs)

-- Exercise 4
-- 4. Turn a list into a palindrome; i.e., it should read the same both backward and
--    forward. For example, given the list [1,2,3], your function should return
--    [1,2,3,3,2,1].

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Exercise 5
-- 5. Write a function that determines whether its input list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs | xs == myReverse xs = True
isPalindrome _                       = False

-- Exercise 6
-- 6. Create a function that sorts a list of lists based on the length of each sublist. (You
--    may want to look at the sortBy function from the Data.List module.)

subcmp a b = compare (myLength a) (myLength b)
sortBySublistLength xs = sortBy subcmp xs

-- Exercise 7 & 8
-- 7. Define a function that joins a list of lists together using a separator value:
--      -- file: ch03/Intersperse.hs
--      intersperse :: a -> [[a]] -> [a]

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ []       = []
myIntersperse _ [x]      = x
myIntersperse sep (x:xs) = x ++ [sep] ++ myIntersperse sep xs


-- Exercise 9
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height Empty = 0
height (Node a left right) | height left > height right = 1 + height left
                           | otherwise                  = 1 + height right

-- Exercise 10
data Point = Point {
       x::Double,
       y::Double
     } deriving (Eq, Show)

data Direction  = DLeft
                | DRight
                | DStraight
                  deriving (Eq, Show)

-- This is my initial attempt at a direction function, improved is below
-- getDirection a b c | pointDifference a b c == 0 = DStraight
--                    | pointDifference a b c  > 0 = if (y b) > (y a) then DLeft else DRight
--                    | otherwise                  = if (y b) < (y a) then DRight else DLeft
--                       where pointDifference a b c  = (y c) - ( ((y b) - (y a))/((x b) - (x a))*((x c) - (x a)) + (y a))

ccw (Point x1 y1) (Point x2 y2) (Point x3 y3) = (x2 - x1)*(y3-y1)-(y2-y1)*(x3-x1)

getDirection a b c | ccw a b c == 0 = DStraight
                   | ccw a b c > 0  = DLeft
                   | otherwise      = DRight

findDirections (x:y:z:[]) = [getDirection x y z]
findDirections (x:y:z:xs) = [getDirection x y z] ++ findDirections ([y]++[z]++xs)

compareY one two | (y one) == (y two) = compare (x one) (x two)
                 | (y one) >  (y two) = GT
                 | otherwise          = LT

cot (Point x1 y1) (Point x2 y2) = (x2-x1)/(y2-y1)

-- I'm not sure this is correct yet. 
sortListByCoTangent p xs = p:(sortBy (compCoTangent p) (delete p (nub xs)))
                                     where
                                      compCoTangent p a b = compare (cot p b) (cot p a)
                                      

grahamScan xs = let p = minimumBy compareY xs
                    lst = (sortListByCoTangent p xs) ++ [p]
                in lst

testData = [(Point 1 1), (Point 2 2), (Point 3 4)]
