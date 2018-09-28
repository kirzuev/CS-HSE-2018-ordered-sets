import Data.Matrix

data Relation = FF | FT | TF deriving (Eq, Show)

instance Num Bool where
    False + False = False
    _     + _     = True

    _ - _ = False

    negate = not

    True * True = True
    _    * _    = False

    abs = id

    signum = id

    fromInteger 0 = False
    fromInteger _ = True

allPairs :: Int -> [(Int, Int)]
allPairs n = pair 1 n

pair :: Int -> Int -> [(Int, Int)]
pair s f
  | s == f    = []
  | otherwise = zip [s,s..] [s+1..f] ++ pair (s+1) f

allRelations :: Int -> [[((Int, Int), Relation)]]
allRelations n = relations (allPairs n)

relations :: [(Int, Int)] -> [[((Int, Int), Relation)]]
relations []     = [[]]
relations (x:xs) = ((\y -> (x,y)) <$> [FF, FT, TF])
  >>= (\r -> (r:) <$> relations xs)

nullMatrix :: Matrix Bool
nullMatrix = matrix 5 5 (\_ -> False)

mkMatrix :: [((Int, Int), Relation)] -> Matrix Bool
mkMatrix rs = mkMatrix' rs nullMatrix

mkMatrix' :: [((Int, Int), Relation)] -> Matrix Bool -> Matrix Bool
mkMatrix' [] m     = m
mkMatrix' (r:rs) m = case r of
  (_, FF)     -> mkMatrix' rs m
  ((i,j), FT) -> mkMatrix' rs (setElem True (i,j) m)
  ((i,j), TF) -> mkMatrix' rs (setElem True (j,i) m)

inM :: Matrix Bool -> Matrix Bool -> Bool
inM m1 m2 = and $
  zipWith (\x y -> x == y || y == True) (toList m1) (toList m2)

isTransitive :: Matrix Bool -> Bool
isTransitive m = multStd2 m m `inM` m

main :: IO ()
main = do
  print $ length $
    filter (\x -> isTransitive x) (mkMatrix <$> (allRelations 5))
