module Test where

import Test.QuickCheck

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = (d + t1 + k + t2 + t3 + 5 * j) `mod` 7
  where
    j :: Integer
    j = y `div` 100
    k :: Integer
    k = y `mod` 100
    m' :: Integer
    m' = if m <= 2 then m+12 else m
    t1 :: Integer
    t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
    t2 :: Integer
    t2 = floor(fromIntegral k / 4.0)
    t3 :: Integer
    t3 = floor(fromIntegral j / 4.0)


sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 0
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m acc
      | y > end = acc
      | otherwise = sundays' nextY nextM remain
      where
        nextY = if m >= 12 then y + 1 else y
        nextM = if m >= 12 then 1 else m + 1
        remain = if dayOfWeek y m 1 == 1 then acc + 1 else acc


daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
  | m == 2                                = if leap y then 29 else 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise                             = 31


leap :: Integer -> Bool
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)


sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1 0 2
  where
    sundays' :: Integer -> Integer -> Integer -> Integer -> Integer
    sundays' y m acc weekday
      | y > end = acc 
      | otherwise = sundays' nextY nextM remain nextWeekday
      where
        nextY  = if m >= 12 then y + 1 else y
        nextM  = if m >= 12 then 1 else m + 1
        nextWeekday = weekday +  (daysInMonth m y) `mod` 7
        remain = if nextWeekday `mod` 7 == 0 then acc + 1 else acc


equal_functions :: Integer -> Integer -> Bool
equal_functions y m = test y m
  where
    test :: Integer -> Integer -> Bool
    test y' m' = sundays1 y' m' == sundays2 y' m'
      where
        y' = if y < 1900 then 1900 else y
        m' = if m <= 0 then 1 else (if m > 12 then 12 else m)


day :: Integer -> Integer -> Integer
day y a 
  | a <= 0    = 0 
  | otherwise = (daysInYear y) + (day (y+1) (a-1))
    where
      daysInYear :: Integer -> Integer
      daysInYear y' = month 1
        where
          month :: Integer -> Integer
          month m 
            | m > 12    = 0 
            | otherwise = (daysInMonth m y') + (month (m+1))


isNumWeeksInteger :: Integer -> Bool
isNumWeeksInteger y = (day y 400) `mod` 7 == 0
