-- You can start directly a program using the runhaskell cmd

-- main :: IO ()
-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main

-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- retrun function perform an IO Action, it doesn't end the program
-- It encapsulate a result, it's some kind of the opposite of the <- operator

main :: IO ()
main = do
  a <- return "Hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b
