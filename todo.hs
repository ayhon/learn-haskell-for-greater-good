import System.Environment
import System.Directory
import System.IO
import Control.Exception
import Data.List

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

main = do
    (command:argList) <- getArgs
    dispatch command argList

usage :: IO ()
usage = putStr "Usage: \n\
\    add fileName todoItem \n\
\    view fileName \n\
\    remove fileName taskNumber \n"

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = usage

view :: [String] -> IO ()
view [fileName]= do
    todoHandle <- openFile fileName ReadMode
    contents <- hGetContents todoHandle
    let tasks = lines contents
        numberedTasks = unlines $ zipWith (\n t -> show n++" - "++t) [0..] tasks
    putStr numberedTasks 
view _ = usage

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    todoHandle <- openFile fileName ReadMode
    contents <- hGetContents todoHandle
    let taskNumber = read numberString
        tasks = lines contents
        filteredTasks = delete (tasks !! taskNumber) tasks
    removeFile fileName
    -- (tempName, tempHandle) <- openTempFile "." "new_todo"
    bracketOnError (openTempFile "." "new_todo") 
            (\(name, handle) -> do
            hClose handle
            removeFile name)
            (\(name, handle) -> do
            hPutStr handle $ unlines filteredTasks
            renameFile name fileName)
remove _ = usage
