myFilter p xs = foldr step [] xs
                where step x ys | p x       = x:ys
                                | otherwise = ys

myMap p xs = foldr step [] xs
             where step x ys = p x : ys

myFoldl f z xs = foldr step (id xs) z
                 where step x g a = g (f a x)
