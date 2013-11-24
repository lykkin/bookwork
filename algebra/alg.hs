linearGCD :: Integer -> Integer -> (Integer, Integer, Integer)
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


reduceMod :: Integer -> Integer -> Integer
reduceMod a n | (a < 0) = reduceMod (a + n) n
	      | (a > n -1) = reduceMod (a - n) n
	      | otherwise = a

multiMod :: Integer -> Integer -> Integer -> Integer
multiMod a b n = reduceMod (a*b) n
 
addMod :: Integer -> Integer -> Integer -> Integer
addMod a b n = reduceMod (a + b) n 
		
getInverse :: Integer -> Integer -> Maybe Integer
getInverse a n = relativePrime where
		 relativePrime | (gcd == 1) = Just (reduceMod inverse n) 
		               | otherwise = Nothing
		 (gcd, inverse, _) = linearGCD a n
