import System.IO

-- Need to close the handle before the end of the function

-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- Avoid the hClose using the withFile function
-- Re implementing withFile

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

main = do
  withFile' "girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)