expandLess :: Int -> [Int]
expandLess n = [0 .. n-1]

toBinaryDigits :: Int -> [Int]
toBinaryDigits 0 = [0]
toBinaryDigits n = reverse (go n)
  where
    go 0 = []
    go x = (x `mod` 2) : go (x `div` 2)

fromBinaryDigits :: [Int] -> Int
fromBinaryDigits = foldl (\acc d -> acc * 2 + d) 0

primeFactors :: Int -> [Int]
primeFactors n = go n 2
  where
    go 1 _ = []
    go m f
      | m `mod` f == 0 = f : go (m `div` f) f
      | otherwise      = go m (f+1)

fibs :: Int -> [Int]
fibs n = take n (map fst (iterate (\(a,b) -> (b,a+b)) (0,1)))

fibsInf :: [Int]
fibsInf = map fst (iterate (\(a,b) -> (b,a+b)) (0,1))

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | even n    = n : collatz (n `div` 2)
  | otherwise = n : collatz (3*n + 1)

sieve :: Int -> [Int]
sieve n = go [2..n]
  where
    go [] = []
    go (p:xs) = p : go [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = sieveInf [2..]
  where
    sieveInf (p:xs) = p : sieveInf [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
    print (expandLess 10)
    print (toBinaryDigits 13)
    print (fromBinaryDigits [1,1,0,1]) -- 13
    print (primeFactors 84)
    print (fibs 10)
    print (take 15 fibsInf)
    print (collatz 13)
    print (sieve 50)
    print (take 20 primes)
