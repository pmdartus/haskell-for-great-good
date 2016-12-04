import Data.Char

-- getContents read the stdin in a lazy manner before printing the caplocked content

main :: IO ()
main = do
  l <- getContents
  putStr $ map toUpper l