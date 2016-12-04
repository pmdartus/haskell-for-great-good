import System.Directory
import System.Environment
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           ]

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName $ todoItem ++ "\n" 

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todos = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todos
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoTasks = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoTasks
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

bump :: [String] -> IO ()
bump [fileName, taskNumStr] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let taskNum = read taskNumStr
        todos = lines contents
        (todoHead,todoEl:todoTail) = splitAt taskNum todos
        newTodos = todoEl : (todoHead ++ todoTail)
    hPutStr tempHandle $ unlines newTodos
    hClose tempHandle
    hClose handle
    removeFile fileName
    renameFile tempName fileName
         

main = do
    (command:args) <- getArgs
    case lookup command dispatch of
        Just action -> action args
        Nothing -> do
            let validCommands = map fst dispatch
            putStr "This command doesn't exists: "
            putStr (show command)
            putStr "\nPlease use: " 
            putStr (intercalate ", " validCommands)
            putStr "\n"
    