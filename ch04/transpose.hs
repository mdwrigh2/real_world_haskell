import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
       where mainWith function = do
               args <- getArgs
               case args of
                    [input, output] -> interactWith function input output
                    _               -> putStrLn "error: exactly two arguments needed"
             myFunction = transpose


removeHeads [] = []
removeHeads (x:xs) = tail' x : removeHeads xs
                     where tail' [] = []
                           tail' xs = tail xs

getHeads [] = []
getHeads (x:xs) = head' x : getHeads xs
                  where head' [] = ' '
                        head' xs = head xs

transpose input | all null (lines input) = []
                | otherwise              = let input' = lines input
                                           in (getHeads input' ++ "\n") ++ transpose (unlines (removeHeads input'))
