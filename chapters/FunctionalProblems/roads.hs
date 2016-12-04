data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadSteps :: (Path, Path) -> Section -> (Path, Path)
roadSteps (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceA = priceA + a
      forwardPriceB = priceB + b
      crossPriceA = priceB + b + c
      crossPriceB = priceA + a + c
      newPathA = if forwardPriceA <= crossPriceA
        then (A,a):pathA
        else (C,c):(B,b):pathB
      newPathB = if forwardPriceB <= crossPriceB
        then (B,b):pathB
        else (C,c):(A,a):pathA
  in (newPathA,newPathB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestPathA, bestPathB) = foldl roadSteps ([], []) roadSystem
  in if sum (map snd bestPathA) <= sum (map snd bestPathB)
    then reverse bestPathA
    else reverse bestPathB