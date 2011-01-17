-- file: WC.hs

-- Exercise 2
wordCount input = show (length (words input)) ++ "\n"

-- Exercise 3
charCount input = show (length input) ++ "\n"

main = interact charCount
