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
-- Exercise 8, 9
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = foldl step [[head xs]] (tail xs)
  where step acc x | f x ((head . last) acc) = (init acc) ++ [(last acc)++[x]]
                   | otherwise         = acc++[[x]]

-- Exercise 10
-- any
-- I actually used an anonymous function here just for practice,
-- read the next couple paragraphs of RWH if you aren't familiar with it
any' f xs = foldr (\x xs -> f x || xs) False xs

-- cycle
-- Again, I used an anonymous function
cycle' xs = foldr (\x acc -> xs ++ acc) [] [1..]


-- words
-- And here I used function composition. Again, this is the next couple
-- pages of RWH. I'd recommend checking them out if you aren't sure what I'm doing.

words' xs = foldr step [[]] xs
  where step x acc | isWhitespace x && (not . null . head) acc = []:acc
                   | isWhitespace x                            = acc
                   | otherwise                                 = (x:(head acc)):(tail acc)

isWhitespace x | x == ' '  = True
               | x == '\n' = True
               | x == '\r' = True
               | x == '\t' = True
               | otherwise = False


-- unlines
-- ...and more anonymous functions
unlines' xs = foldr (\x ys -> (x++"\n")++ys ) "" xs
