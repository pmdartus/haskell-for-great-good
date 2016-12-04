import Data.Char

-- Compile via: ghc --make helloworld
-- Use `do` to glue IO actions together

-- The <- allow to unbox the IO result value
-- getline as IO String type


main = do
  putStrLn "What's your firstname?"
  firstname <- getLine
  putStrLn "What's your lastname?"
  lastname <- getLine
  let
    bigFirstName = map toUpper firstname
    bigLastName = map toUpper lastname
  putStrLn $ "Hey " ++ bigFirstName ++ bigLastName ++ ", nice to meet you"