import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add)
  ,("view", view)
  ,("remove", remove)]

main = do
  (command: args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


view :: [String] -> IO ()
view [fileName] = withFile fileName ReadMode (\handle -> do
  contents <- hGetContents handle
  let todoTasks = lines contents
  mapM_ putStrLn $ zipWith (\n line->show n ++ " - " ++ line) [0..] todoTasks
  )

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")



remove :: [String] -> IO ()
remove [ fileName, numString ] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

