-- Exercise 01
-- Write your own “safe” definitions of the standard partial list functions, but make
-- sure they never fail.

-- First couple to show how it can be done without using the stdlib functions
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

-- These are easier to do with std. lib. functions, but obviously can be done without
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- Exercise 02
-- Write a function splitWith that acts similarly to words but takes a predicate and a
-- list of any type, and then splits its input list on every element for which the predicate
-- returns False

splitWith :: (a -> Bool) -> [a] -> [[a]]
