module Template.Math.Prime ()
where

import Template.Prelude

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], mod n x == 0]
-- primes = [ x,  y <- integers, y mod ]

isPrimes n = factors n == [1, n]

primes = [x | x <- [1..], isPrimes x]

pgcd m n = maximum $ [x | x <- factors m, x `elem` (factors n)]
