module Question4 where

daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
  | m == 2                                = if leaf y then 29 else 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise                             = 31


leap :: Integer -> Bool
leap y = (y `mod` 4 == 0) && (y `mod` 100 != 0) || (y `mod` 400 == 0)


sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1 0
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m acc
      | y > end = acc 
      | otherwise = sundays' nextY nextM remain
      where
        nextY = if m >= 12 then y + 1 else y
        nextM = if m >= 12 then 1 else m + 1
        remain = if dayOfWeek y m 1 == 1 then acc + 1 else acc
