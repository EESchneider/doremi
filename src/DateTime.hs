{-# LANGUAGE OverloadedStrings #-}

module DateTime
  ( Date(..)
  , Time(..)
  , DateTime(..)
  , TimeDelta(..)
  , now
  , plusDelta
  , minusDelta
  , backwardsDelta
  ) where

import Prelude hiding (lookup)
import qualified Prelude as P (lookup)
import Control.Lens
import Data.Aeson.Types (typeMismatch)
import Data.HashMap.Strict (empty, insert, lookup, (!))
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (unpack, pack, splitOn, Text)
import Data.Yaml
import Data.Char (isDigit)
import Data.List (partition)
import System.Process (shell, readCreateProcess) -- to get the current time
import Text.Read (readMaybe)
import Control.Exception (catch)

data Date = Date { day :: Int, month :: Int, year :: Int}
  deriving (Eq, Ord)
data Time = Time { minute :: Int, hour :: Int }
  deriving (Eq, Ord)
data DateTime = DateTime { timePart :: Time, datePart :: Date}
  deriving (Eq, Ord)
data TimeDelta = TimeDelta { minutes :: Int, hours :: Int, days :: Int }
  deriving (Eq, Ord)

instance Show Date where
  show (Date d m y) = show y ++ "-" ++ show m ++ "-" ++ show d
instance Show Time where
  show (Time m h) = paddedMin ++ ":" ++ paddedHour
    where paddedMin
            | m < 10 = '0' : show m
            | otherwise = show m
          paddedHour
            | h < 10 = '0' : show h
            | otherwise = show h
instance Show DateTime where
  show (DateTime (Time 0 0) date) = show date
  show (DateTime time date) = show date ++ " " ++ show time
instance Show TimeDelta where
  show (TimeDelta 0 0 0) = ""
  show (TimeDelta 0 0 d) = (show d) ++ "d"
  show (TimeDelta 0 h d) = (show h) ++ "h" ++ (show $ TimeDelta 0 0 d)
  show (TimeDelta m h d) = (show m) ++ "m" ++ (show $ TimeDelta 0 h d)

instance Read Date where
  readsPrec _ s = let parts = map unpack $ splitOn "-" $ pack s
                      isGood
                        | all (all isDigit) parts && length parts == 3 = True
                        | otherwise = False
                   in if isGood
                         then [(Date (read $ parts !! 0) (read $ parts !! 1) (read $ parts !! 2), "")]
                         else []
instance Read Time where
  readsPrec _ "" = [(Time 0 0, "")]
  readsPrec _ s = let parts = map unpack $ splitOn ":" $ pack s
                      isGood
                        | all (all isDigit) parts && length parts == 2 = True
                        | otherwise = False
                   in if isGood
                         then [(Time (read $ parts !! 1) (read $ parts !! 0), "")]
                         else []
instance Read DateTime where
  readsPrec _ s = let parts = map unpack $ splitOn " " $ pack s
                      isGood
                        | length parts == 2 && (
                            readMaybe (parts !! 0) == (Nothing :: Maybe Date)
                            || readMaybe (parts !! 1) == (Nothing :: Maybe Time)) = False
                        | length parts == 1
                          && readMaybe (parts !! 0) == (Nothing :: Maybe Date) = False
                        | otherwise = True
                   in if isGood
                         then [(DateTime (read $ parts !! 1) (read $ parts !! 0), "")]
                         else []
instance Read TimeDelta where
  readsPrec _ "" = [(TimeDelta 0 0 0, "")]
  readsPrec _ s = let (x, (unit:rest)) = partition isDigit s
                      v = read x
                      delta = case unit of
                                'm' -> TimeDelta v 0 0
                                'h' -> TimeDelta 0 v 0
                                'd' -> TimeDelta 0 0 v
                   in [(delta `plusDelta` read rest, rest)]

instance ToJSON DateTime where
  toJSON = String . pack . show

instance ToJSON TimeDelta where
  toJSON = String . pack . show

-- |Adds sensible defaults if any fields are missing
instance FromJSON DateTime where
  parseJSON = withObject "DateTime" $ \object -> do
    -- unprocessed variables
    -- time components will be assumed to be the beginning of the day
    min' <- object .:? "minute"
    h' <- object .:? "hour"
    d' <- object .: "day"
    month' <- object .: "month"
    y' <- object .: "year"
    
    -- parsed variables
    let min = fromScientific $ readOr 0 min'
    let h = fromScientific $ readOr 0 h'
    let d = fromScientific d'
    let month = fromScientific month'
    let y = fromScientific y'

    return $ DateTime (Time min h) (Date d month y)
      where readOr def x = fromMaybe def $ fmap read x
            fromScientific = truncate . toRealFloat

instance FromJSON TimeDelta where
  parseJSON = withObject "TimeDelta" $ \object -> do
    mins  <- withDefault 0 object "minutes"
    hours <- withDefault 0 object "hours"
    days  <- withDefault 0 object "days"

    return $ TimeDelta mins hours days
      where withDefault def obj name = (fromMaybe def . (fmap fromScientific)) <$> obj .:? name
            fromScientific = truncate . toRealFloat

-- | 1-indexed conversion from month (1 = Jan, 2 = Feb, ..., 11 = Nov, 12 = Dec) to the number of days in it
daysInMonth :: Int -> Int -> Int
daysInMonth y 2
  | isLeapYear y = 29
  | otherwise = 28
daysInMonth y m = daysInMonthMap ! m
  where
    daysInMonthMap = foldr (uncurry insert) empty
      [ (1, 31)
      -- no february because leap years are comparatively complicated
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

class Deltable a where
  plusDelta :: a -> TimeDelta -> a
  minusDelta :: a -> TimeDelta -> a
  minusDelta x = plusDelta x . backwardsDelta

instance Deltable DateTime where
  (DateTime timePart datePart) `plusDelta` (TimeDelta minutes hours days) =
    let timePart' = Time { minute = minute timePart + minutes
                         , hour = hour timePart + hours }
        datePart' = datePart { day = day datePart + days }
     in normalize $ DateTime timePart' datePart'

instance Deltable TimeDelta where
  (TimeDelta m h d) `plusDelta` (TimeDelta m' h' d') = normalize $ TimeDelta (m + m') (h + h') (d + d')

backwardsDelta :: TimeDelta -> TimeDelta
backwardsDelta (TimeDelta m h d) = TimeDelta (negate m) (negate h) (negate d)

isLeapYear :: Int -> Bool
isLeapYear y -- algorithm taken directly from en.wikipedia.org/wiki/Leap_year#Algorithm
  | y `mod` 4 /= 0 = False
  | y `mod` 100 /= 0 = True
  | y `mod` 400 /= 0 = False
  | otherwise = True

class MultipleUnits a where
  normalize :: a -> a

instance MultipleUnits DateTime where
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

instance MultipleUnits TimeDelta where
  normalize (TimeDelta 0 0 0) = TimeDelta 0 0 0
  normalize x@(TimeDelta m 0 0)
    | m < 60 = x
    | otherwise = TimeDelta (m `mod` 60) (m `div` 60) 0
  normalize x@(TimeDelta 0 h 0)
    | h < 24 = x
    | otherwise = TimeDelta 0 (h `mod` 24) (h `div` 24)
  normalize x@(TimeDelta 0 0 d) = x
  normalize (TimeDelta m h d) = foldl (\x -> normalize . plusDelta x)
                                      (TimeDelta 0 0 0) [ TimeDelta m 0 0
                                                        , TimeDelta 0 h 0
                                                        , TimeDelta 0 0 d
                                                        ]

now :: IO DateTime
now = do
  let dateCommand = shell "date '+%-Y %-m %-d %-H %-M'"
  dateOutput <- readCreateProcess dateCommand ""
  let year:month:day:hour:minute:[] = (map (read . unpack) $ splitOn " " (pack dateOutput)) :: [Int]
  return DateTime { timePart = Time { hour = hour, minute = minute }
                  , datePart = Date { year = year, month = month, day = day } }
