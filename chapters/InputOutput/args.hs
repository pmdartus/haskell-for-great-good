import System.Environment
import Data.List


main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The args are:"
  mapM putStrLn args
  putStrLn "The prog is:"
  putStrLn progName
