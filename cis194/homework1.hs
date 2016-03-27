--http://www.seas.upenn.edu/%7Ecis194/spring13/hw/01-intro.pdf
toDigitsRev :: Integer -> [Integer]
toDigitsRev  x | x <= 0 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)


toDigits :: Integer -> [Integer]
toDigits x = reverse.toDigitsRev $ x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | length(xs) `mod` 2 == 1 = 2*x : doubleEveryOther(xs)
  | length(xs) `mod` 2 == 0 = x : doubleEveryOther(xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum $ toOneDigit xs

toOneDigit :: [Integer] -> [Integer]
toOneDigit [] = []
toOneDigit (x:xs) = toDigits x ++ toOneDigit(xs)

validate:: Integer -> Bool

validate x = let xs = doubleEveryOther.toDigits $ x
                 remainder = sumDigits xs `mod` 10
              in remainder == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c  | n <= 0 = []
      | otherwise = hanoi (n-1) a c b ++ [(a,c)] ++ hanoi (n-1) b a c

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c d
  | n <= 0 = []
  | n == 1 = [(a,d)]
  | n == 3 = [(a,b), (a,c), (a,d), (c,d), (b,d)]
  | otherwise = hanoi' (n-2) a d c b ++ [(a,c), (a,d), (c,d)] ++ hanoi'(n-2) b a c d

{-|
hanoi'' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi'' n a b c d
  | n <= 0 = []
  | n == 1 = [(a,d)]
  | otherwise = hanoi ((n-2)`quot`2) a b c ++ hanoi((n-2)`quot`2 +1) a d b ++ [(a,d)] ++ hanoi((n-2)`quot`2) c a d ++ hanoi'' ((n-2)`quot`2 + 1) b a c d
-}
