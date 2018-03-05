module Main where
  
import Data.Yaml
import Data.Functor
import System.IO (hPrint, stderr)
import System.Environment (getArgs)
import Todo (Completion(..), Todo(..), newTodo)
import DateTime

todoCircle :: Todo -> String
todoCircle t = case (isDone t) of
                 Done       -> "\xE95f"
                 InProgress -> "\xE960"
                 Undone     -> "\xE961"

writeTodo :: FilePath -> [Todo] -> IO ()
writeTodo = encodeFile

getTodo :: FilePath -> IO [Todo]
getTodo path = do
  itemsMaybe <- decodeFile path
  case itemsMaybe of
    Just items -> return items
    Nothing    -> error $ "Could not read from " ++ path

addTodo :: FilePath -> Todo -> IO ()
addTodo path todo = do
  items <- getTodo path
  writeTodo path $ todo : items

shortPrint :: [Todo] -> IO ()
shortPrint = putStrLn . unwords . map todoCircle

shortPrintFile :: FilePath -> IO ()
shortPrintFile f = getTodo f >>= shortPrint

main = do
  allArgs <- getArgs
  let (action:args) = if allArgs == [] then ["get"]
                                       else allArgs

  let defaultPath = "todos/doremi.yaml" :: FilePath
  let path = defaultPath

  case action of
    "get" -> shortPrintFile path
    "add" -> let title = args !! 0
                 desc = args !! 1
                 item = newTodo title desc $ DateTime (Time 0 0) (Date 0 0 0)
              in addTodo path item
                 
    _ -> error "TODO: usage"
