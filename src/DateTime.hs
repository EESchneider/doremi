module DateTime where

import Data.HashMap.Strict hiding (foldr, map)
import Data.Scientific (scientific)
import System.Process (shell, readCreateProcess) -- to get the current time
import Data.Text (pack, unpack, splitOn)
import Data.Yaml

data Date = Date { day :: Int, month :: Int, year :: Int}
  deriving (Eq, Ord, Show)
data Time = Time { minute :: Int, hour :: Int }
  deriving (Eq, Ord, Show)
data DateTime = DateTime { timePart :: Time, datePart :: Date}
  deriving (Eq, Ord)
data TimeDelta = TimeDelta { minutes :: Int, hours :: Int, days :: Int }
  deriving (Eq, Ord)

instance Show DateTime where
  show (DateTime { timePart = t, datePart = d }) = show (year d) ++ "-" ++ show (month d) ++ "-" ++ show (day d) ++ " " ++ show (hour t) ++ ":" ++ show (minute t)
instance Show TimeDelta where
  show (TimeDelta { minutes = m, hours = h, days = d }) = show d ++ "d" ++ show h ++ "h" ++ show m ++ "m"

instance ToJSON DateTime where
  toJSON (DateTime t d) = Object $ foldr (uncurry insert) empty
    [ (pack "minute", number $ minute t)
    , (pack "hour", number $ hour t)
    , (pack "day", number $ day d)
    , (pack "month", number $ month d)
    , (pack "year", number $ year d)]
      where number c = Number $ scientific (fromIntegral c) 1

instance ToJSON TimeDelta where
  toJSON (TimeDelta m h d) = Object $ foldr (uncurry insert) empty
    [ (pack "minutes", number m)
    , (pack "hours", number h)
    , (pack "days", number d)]
      where number c = Number $ scientific (fromIntegral c) 1

-- TODO FromJSON

-- | 1-indexed conversion from month (1 = Jan, 2 = Feb, ..., 11 = Nov, 12 = Dec) to the number of days in it
daysInMonth :: Int -> Int -> Int
daysInMonth y 2
  | isLeapYear y = 29
  | otherwise = 28
daysInMonth y m = daysInMonthMap ! m
  where
    daysInMonthMap = foldr (uncurry insert) empty
      [ (1, 31)
      , (3, 31)
      , (4, 30)
      , (5, 31)
      , (6, 30)
      , (7, 31)
      , (8, 31)
      , (9, 30)
      , (10, 31)
      , (11, 30)
      , (12, 31)
      ]

plusDelta :: DateTime -> TimeDelta -> DateTime
(DateTime timePart datePart) `plusDelta` (TimeDelta minutes hours days) =
  let timePart' = Time { minute = minute timePart + minutes
                       , hour = hour timePart + hours }
      datePart' = datePart { day = day datePart + days }
   in normalize $ DateTime { timePart = timePart', datePart = datePart' }

minusDelta :: DateTime -> TimeDelta -> DateTime
minusDelta dateTime delta = dateTime `plusDelta` backwardsDelta delta

backwardsDelta :: TimeDelta -> TimeDelta
backwardsDelta (TimeDelta m h d) = TimeDelta (negate m) (negate h) (negate d)

isLeapYear :: Int -> Bool
isLeapYear y -- algorithm taken directly from en.wikipedia.org/wiki/Leap_year#Algorithm
  | y `mod` 4 /= 0 = False
  | y `mod` 100 /= 0 = True
  | y `mod` 400 /= 0 = False
  | otherwise = True

normalize :: DateTime -> DateTime
normalize x@(DateTime { timePart = t, datePart = d })
  | minute t >= 60 ||
    minute t < 0 = let minute' = minute t `mod` 60
                       hour' = hour t + minute t `div` 60
                       timePart' = t { minute = minute', hour = hour' }
                    in normalize $ x { timePart = timePart' }
  | hour t >= 24 ||
    hour t < 0 = let hour' = hour t `mod` 24
                     day' = day d + hour t `div` 24
                     timePart' = t { hour = hour' }
                     datePart' = d { day = day' }
                  in normalize $ x { timePart = timePart', datePart = datePart' }
  | month d > 12 ||
    month d <= 0 = let zeroIndexedMonth = month d - 1
                       zeroIndexedMonth' = zeroIndexedMonth `mod` 12
                       month' = zeroIndexedMonth' + 1
                       year' = year d + year d `div` 12
                       datePart' = d { month = month', year = year' }
                    in normalize $ x { datePart = datePart' }
  | day d > daysInMonth (year d) (month d) = let day' = day d - daysInMonth (year d) (month d)
                                                 month' = month d + 1
                                                 datePart' = d { day = day', month = month' }
                                              in normalize $ x { datePart = datePart' }
  | day d <= 0 = let day' = day d + daysInMonth (year d) (month d)
                     month' = month d - 1
                     datePart' = (datePart x) { day = day', month = month' }
                  in normalize $ x { datePart = datePart' }
  | otherwise = x

now :: IO DateTime
now = do
  let dateCommand = shell "date '+%-Y %-m %-d %-H %-M'"
  dateOutput <- readCreateProcess dateCommand ""
  let year:month:day:hour:minute:_ = map unpack $ splitOn (pack " ") (pack dateOutput)
  return DateTime { timePart = Time { hour = read hour, minute = read minute }
                  , datePart = Date { year = read year, month = read month, day = read day } }
