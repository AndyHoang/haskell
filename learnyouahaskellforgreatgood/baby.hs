import Data.Char
import qualified Data.Map as Map
doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x >100
                         then x
                         else x*2

length' xs = sum[1| _ <- xs]

removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

removeOdd xxs = [[x | x <- xs, even x] | xs <- xxs]


capital :: String -> String
capital "" = "empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ "is " ++ [x]
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a== b = EQ
  | otherwise = LT

initials :: String -> String -> String
initials firstName lastName = [f] ++ " . " ++ [l] ++ ". "
  where (f:_) = firstName
        (l:_) = lastName

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi |(w,h) <- xs, let bmi = w/h^2]

replicate' :: (Num i, Ord i) => i -> a -> [a]

replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' 0 x = []
replicate'' n x = x : replicate''(n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]

take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++  [x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x: xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]

quicksort [] = []
quicksort (x:xs) = let smallerSorted = quicksort [a | a <- xs, a <= x]
                       biggerSorted = quicksort [ a | a <- xs, a > x ]
                  in smallerSorted ++ [x] ++ biggerSorted


zipWith' :: (a->b->c) -> [a] -> [b] -> [c]

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

countSumRootGT1000 :: Int
countSumRootGT1000 = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))+ 1


oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000). filter odd . map (^2) $ [1..]

oddSquareSum' :: Integer
oddSquareSum' = let oddSquares =  filter odd $ map (^2) [ 1.. ]
                    belowLimit = takeWhile (<10000)
                 in sum . belowLimit $ oddSquares

encode shift = map (chr . (+ shift) . ord)

decode shift = encode (negate shift)


findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):sx) = case (key==k) of
                           True -> Just v
                           False -> findKey key (sx)

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key xs = foldr (\(k,v) acc -> if (key == k) then Just v else acc) Nothing xs

data Point = Float Float


add :: [String] -> String
add [a, b] = a ++ b


