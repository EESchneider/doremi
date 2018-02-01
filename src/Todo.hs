module Todo where

import Data.Yaml
import Data.Text (pack)
import DateTime

data Todo = Todo { todoTitle :: String
                 , todoDesc :: String
                 , isCompleted :: Bool
                 , deadline :: DateTime
                 , completionTime :: TimeDelta
                 , subtasks :: [Todo]
                 }
                 deriving (Eq)

instance ToJSON Todo where
  toJSON (Todo title desc complete dead estTime subs) = Object $
    foldr (uncurry insert) empty [("todoTitle", String $ pack title)
                                 , ("todoDesc", String $ pack desc)
                                 , ("isCompleted", Bool complete)
                                 , ("deadline", toJSON dead) -- TODO
                                 , ("completionTime", toJSON estTime) -- TODO
                                 , ("subtasks", Array $ map (pack . todoTitle) subtasks)]

-- | Quickly create a simple todo item
todo :: String -> String -> DateTime -> TimeDelta -> Todo
todo title desc dead completion = Todo { todoTitle = title
  , todoDesc = desc
  , deadline = dead
  , completionTime = completion
  , isCompleted = False
  , subtasks = []
  }

isUrgent :: Todo -> IO Bool
isUrgent x = do
  now' <- now
  return $ (deadline x `minusDelta` completionTime x) <= now'
