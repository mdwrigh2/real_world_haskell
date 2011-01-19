--Using the command framework from the earlier section “A Simple Command-Line
--Framework” on page 71, write a program that prints the first word of each line of
--its input.

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
               [input, output] -> interactWith function input output
               _ -> putStrLn "error: exactly two arguments needed"
        myFunction = firstWord

firstWord input = helper (lines input)
                  where helper (x:xs) = head' (words x) ++ "\n" ++ helper xs
                          where head' [] = []
                                head' xs = head xs
                        helper _ = []
