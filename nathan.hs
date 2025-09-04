import Data.List ((\\))  -- (\\) is set-difference for unordered lists
primes :: [Integer]
primes = 2: 3: calcNextPrimes (tail primes) [5, 7 .. ]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates in
      smallerSquareP ++ calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
x = zipWith (\a b -> (a,b-a)) primes (tail primes)

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []
  | otherwise = factorize n 2

factorize :: Integer -> Integer -> [Integer]
factorize n d
  | d * d > n = [n] -- If d*d > n, then n must be prime (or 1), so it's the last factor
  | n `mod` d == 0 = d : factorize (n `div` d) d
  | otherwise = factorize n (d + 1)
bigs :: Ord b => b -> [(a,b)] -> [(a,b)]
bigs a (b:c) = if a >= snd b then bigs a c else b : bigs (snd b) c
