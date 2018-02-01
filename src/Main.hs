module Main where
  
import DateTime (now)
import Data.Yaml.Builder (toYaml)

main :: IO ()
main = do
  now' <- now
  let t = todo "Aka!" "Many things are simonesque" now' (TimeDelta 0 0 0)
  print $ toYaml t
