-- Print only short lines

-- Uses interact to takes input a strng and print back strings on
-- the terminal

-- main = do
--   contents <- getContents
--   putStr $ shortLinesOnly contents

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\a -> length a < 10) allLines
      results = unlines shortLines
  in results