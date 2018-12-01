import Data.Matrix hiding (toList)
import Data.Vector (toList)
import Data.List (intersect, sort)

inputMatrix :: Matrix Bool
inputMatrix = fromLists
  [ [  True,  True,  True,  True,  True, False,  True,  True,  True ]
  , [ False,  True, False,  True, False, False, False, False,  True ]
  , [ False,  True, False,  True, False, False, False, False,  True ]
  , [ False, False, False,  True, False, False, False, False, False ]
  , [ False,  True,  True,  True,  True,  True, False, False,  True ]
  , [ False,  True, False, False, False, False, False, False, False ]
  , [  True,  True,  True,  True,  True,  True,  True, False,  True ]
  , [  True, False,  True, False, False, False,  True,  True,  True ]
  , [  True,  True, False,  True, False,  True, False, False,  True ]
  , [ False,  True,  True,  True,  True,  True, False, False,  True ]
  , [  True,  True, False,  True, False, False, False,  True,  True ]
  , [  True,  True,  True,  True, False, False,  True,  True,  True ]
  , [  True,  True, False,  True, False,  True,  True, False,  True ]
  ]

getAllBSets :: Matrix Bool -> [[Int]]
getAllBSets m = [] : foldr go (const []) [1..ncols m] [[]]
  where go x f a = let b = (x:) <$> a in b ++ f (a ++ b)

down :: Matrix Bool -> [Int] -> [Int]
down m bs = as
  where
    bRows = (\col -> zip [1,2..] $ toList $ getCol col m) <$> bs
    as    = foldr intersect [1..nrows m] $ (\a -> fst <$> filter snd a) <$> bRows

up :: Matrix Bool -> [Int] -> [Int]
up m as = bs
  where
    aCols = (\row -> zip [1,2..] $ toList $ getRow row m) <$> as
    bs    = foldr intersect [1..ncols m] $ (\b -> fst <$> filter snd b) <$> aCols

eq :: [Int] -> [Int] -> Bool
eq s1 s2 = sort s1 == sort s2

getFormalConcepts :: Matrix Bool -> [([Int], [Int])]
getFormalConcepts m = fst <$> filter snd pairs
  where
    bs    = getAllBSets m
    as    = down m <$> bs
    pairs = zipWith (\a b -> ((sort a, sort b), eq (up m a) b)) as bs

showFCList :: [([Int], [Int])] -> String
showFCList []         = ""
showFCList ((a,b):xs) = "A = " ++ show a ++ ", B = " ++ show b ++ "\n" ++ showFCList xs

main :: IO ()
main = do
  let formalConcepts = getFormalConcepts inputMatrix
  putStrLn $ showFCList formalConcepts
  putStrLn $ "Number of pairs = " ++ show (length formalConcepts)