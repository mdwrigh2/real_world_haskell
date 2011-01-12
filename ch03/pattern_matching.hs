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
     } deriving (Show)

data Direction  = DLeft
                | DRight
                | DStraight
                  deriving (Eq, Show)

getDirection a b c | pointDifference a b c == 0 = DStraight
                   | pointDifference a b c  > 0 = if (y b) > (y a) then DLeft else DRight
                   | otherwise                  = if (y b) < (y a) then DRight else DLeft
                      where pointDifference a b c  = (y c) - ( ((y b) - (y a))/((x b) - (x a))*((x c) - (x a)) + (y a))
