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
    t2 = floor(k / 4.0)
    t3 :: Integer
    t3 = floor(j / 4.0)

