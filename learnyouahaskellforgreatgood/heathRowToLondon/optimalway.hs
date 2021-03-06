import Data.List

data Node = Node Road (Maybe Road)
data Road = Road Int Node

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
   in if sum (map snd bestAPath) <= sum (map snd bestBPath)
         then reverse bestAPath
         else reverse bestBPath


roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      crossPriceToA = priceB + b + c
      newPathToA = if forwardPriceToA <= crossPriceToA
                      then (A, a): pathA
                      else (C, c): (B,b):pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
                      then (B,b):pathB
                      else (C,c):(A,a):pathA
                   in (newPathToA, newPathToB)

groupOf :: Int -> [a] -> [[a]]
groupOf 0 _ = undefined
groupOf _ [] = []
groupOf n xs = take n xs : groupOf n (drop n xs)


main = do
  contents <- getContents
  let threes = groupOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show .  fst) path
      pathPrice = sum $ map snd path 
   in putStrLn $ "bestpath : " ++ pathString ++ " with price " ++  show pathPrice




