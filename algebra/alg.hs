linearGCD :: Int -> Int -> (Int, Int, Int)
linearGCD a b = (d, u, v) where
    (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
    u | a < 0     = negate x
      | otherwise = x
    v | b < 0     = negate y
      | otherwise = y
    eGCD n1 o1 n2 o2 r s
      | s == 0    = (r, o1, o2)
      | otherwise = case r `quotRem` s of
                      (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t
