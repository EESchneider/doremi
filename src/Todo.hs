{-# LANGUAGE OverloadedStrings #-}

module Todo
  ( Completion(..)
  , Todo(..)
  , newTodo
  , isUrgent
  ) where

import Control.Monad ((=<<))
import Data.Maybe
import Data.Functor ((<$>))
import Data.Yaml
import Data.Text (pack)
import Data.Vector (empty, cons)
import DateTime

data Completion = Undone | InProgress | Done
  deriving (Show, Read, Eq, Enum)

instance ToJSON Completion where
  toJSON Undone = String "Undone"
  toJSON InProgress = String "InProgress"
  toJSON Done = String "Done"

instance FromJSON Completion where
  parseJSON = withText "Completion" $ \text -> return $ case text of
                                                        "Undone" -> Undone
                                                        "InProgress" -> InProgress
                                                        "Done" -> Done

data Todo = Todo { todoTitle :: String
                 , todoDesc :: String
                 , isDone :: Completion
                 , deadline :: DateTime
                 , completionTime :: TimeDelta
                 , subtasks :: [Todo]
                 }
                 deriving (Eq, Show)

instance ToJSON Todo where
  toJSON (Todo title desc done dead time tasks) = object [ ("todo item", object
    [ ("title", String $ pack title)
    , ("description", String $ pack desc)
    , ("done", toJSON done)
    , ("deadline", toJSON dead)
    , ("completion time", toJSON time)
    , ("subtasks", (Array . listToVec) $ map toJSON tasks)
    ]) ]
      where listToVec = foldr cons empty

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ (>>= parseInnerJSON) . (.: "todo item")
    where parseInnerJSON = withObject "Todo" $ \object -> do
                              title <- object .: "title"
                              description <- fromMaybe "" <$> (object .:? "description")
                              done <- parseJSON =<< object .: "done"
                              dead <- parseJSON =<< object .: "deadline"
                              time <- parseJSON =<< object .: "completion time"
                              tasks <- object .: "subtasks"

                              return $ Todo title description done dead time tasks

newTodo :: String -> String -> DateTime -> Todo
newTodo title desc datetime = Todo title desc Undone datetime (TimeDelta 0 0 0) []

isUrgent :: Todo -> IO Bool
isUrgent x = do
  now' <- now
  return $ (deadline x `minusDelta` completionTime x) <= now'

completionStatus :: Todo -> Completion
completionStatus (Todo { isDone = c, subtasks = s })
  | c == Done = Done
  | length s == 0 = Undone
  | all (== Done) $ map completionStatus s = Done
  | all (== Undone) $ map completionStatus s = Undone
  | otherwise = InProgress
