module Sundays where

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
