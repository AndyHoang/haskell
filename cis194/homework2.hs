{-# OPTIONS_GHC -Wall #-}
module HW02 where
--http://www.seas.upenn.edu/%7Ecis194/hw/02-lists.pdf
--NEED TO IMPROVE MORE
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys)
  | x==y = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys



-- Exercise 2 ---------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColor :: Peg ->Code ->Int
countColor x ys = foldl (\acc  y -> if y == x then 1 + acc else acc ) 0 ys

countColors :: Code -> [Int]
countColors xs = map (\color -> countColor color xs) colors



-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = let xs' = countColors xs
                    ys' = countColors ys
                 in totalMin xs' ys'


totalMin :: [Int] -> [Int] -> Int
totalMin _ [] = 0
totalMin [] _ = 0
totalMin (x:xs) (y:ys) = min x y +  totalMin xs ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys = let exact = exactMatches xs ys
                    nonExact= matches xs ys - exact
                 in Move ys exact nonExact

-- Exercise 4 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move code match nonMatch) xs = let Move _ i j = getMove code xs
                                              in (i, j) == (match, nonMatch)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m sx = filter (\code -> isConsistent m code) sx

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = foldr (\x acc -> [[x]]++acc) [] colors
allCodes n = concatMap (\x -> addCode x)$ allCodes(n-1)

addCode :: Code -> [Code]
addCode c = concatMap(\peg -> [c++[peg]]) colors

filterAllCode:: Move -> [Code]
filterAllCode m@(Move guess _ _) = filter (\code -> code /= guess) ( filterCodes m $ allCodes (length guess))

filterAllMoves :: Code -> Move -> [Move]
filterAllMoves secret m =
  map (\code -> getMove secret code )$ filterAllCode m


--getSuficientMove:: Move -> [Code] -> [Move]
--getSuficientMove m@(Move guess match unMatch) xs = scanl () [] xs
-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = let x@(Move guess match _) = getMove c (take (length c) (repeat Red))
           in x:subSolve c x


subSolve:: Code -> Move -> [Move]
subSolve secret m@(Move guess match _ )
  | match == length(secret) = []
  | otherwise =
    let group = filterAllCode m
        sameMoves = movesUntilNextSuficient secret match group
        goodMove = last sameMoves
     in sameMoves ++ subSolve secret goodMove


movesUntilNextSuficient :: Code -> Int -> [Code] -> [Move]
movesUntilNextSuficient secret currMatch codes = takeWhile' (\move -> foo move currMatch) (map (\code -> getMove secret code) codes)
movesUntilNextSuficient' :: Code -> Int -> [Move] -> [Move]
movesUntilNextSuficient' secret currMatch moves=
  takeWhile' (\move -> foo move currMatch) moves


takeWhile':: (a->Bool) ->[a] ->[a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x: takeWhile' p xs
  | otherwise = [x]
foo :: Move -> Int -> Bool
foo (Move _ matching _) currMatch =  matching <= currMatch

-- Bonus ----------------------------------------------

--fiveGuess :: Code -> [Move]
--fiveGuess = undefined
