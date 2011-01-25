import Data.Char (digitToInt) -- Needed for asInt
import Data.List (foldl')
myFilter p xs = foldr step [] xs
                where step x ys | p x       = x:ys
                                | otherwise = ys

myMap p xs = foldr step [] xs
             where step x ys = p x : ys

myFoldl f z xs = foldr step id xs z
                 where step x g a = g (f a x)


-- Folds (pg. 97)
-- Exercise 1 - 4
-- The type has to be defined so fromIntegral can infer what type conversion needs to be
-- performed.
asInt :: String -> Int
asInt ('-':xs) = -(asInt xs)
asInt [] = error "Not a valid numeral!"
asInt xs | val >= min && val <= max = fromIntegral val
         | otherwise                = error "Number exceeds bounds of Int"
  where max = toInteger (maxBound::Int)
        min = toInteger (minBound::Int)
        val = foldl' loop 0 xs
          where loop acc x  = acc*10 + toInteger (digitToInt x)

-- Exercise 5, 6
append xs ys = foldr (:) ys xs
concat' :: [[a]] -> [a]
concat' xs = foldr append [] xs

-- Exercise 7
-- Excplicit recursion version
takeWhile' f (x:xs) | f x       = x : takeWhile f xs
                    | otherwise = []
takeWhile' _ [] = []

-- Folding version
takeWhile_folded :: (a -> Bool) -> [a] -> [a]
takeWhile_folded f (xs) = foldr step [] xs
  where step x acc | f x       = x : acc
                   | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = foldl step [[head xs]] (tail xs)
  where step acc x | f x ((head . last) acc) = (init acc) ++ [(last acc)++[x]]
                   | otherwise         = acc++[[x]]

