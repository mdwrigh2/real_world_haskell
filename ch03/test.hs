type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]
takeTwo :: [a] -> [a]
takeTwo (x:y:ys) = [x,y]
