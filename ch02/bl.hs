beforeLast :: [a] -> a
beforeLast xs = if length xs <= 2
                then head xs
                else beforeLast (tail xs)
