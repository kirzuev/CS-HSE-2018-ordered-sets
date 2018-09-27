import Data.Matrix
import Control.DeepSeq (force)

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

matrices5x5 :: [Matrix Bool]
matrices5x5 = fromLists <$> allMatrices 5 5

allMatrices :: Int -> Int -> [[[Bool]]]
allMatrices 0 _ = []
allMatrices 1 k = (\x -> [x]) <$> allRows k
allMatrices n k = concat $ (\row -> (\matr -> row : matr) <$> tailMatrices) <$> allRows k
  where
    tailMatrices = allMatrices (n-1) k

allRows :: Int -> [[Bool]]
allRows 0 = []
allRows 1 = [[False], [True]]
allRows n = ((\x -> False : x) <$> tailRows) ++ ((\x -> True : x) <$> tailRows)
  where
    tailRows = allRows (n-1)

inM :: Matrix Bool -> Matrix Bool -> Bool
inM m1 m2 = and $
  zipWith (\x y -> x == y || y == True) (toList m1) (toList m2)

isTransitive :: Matrix Bool -> Bool
isTransitive m = (force (multStd2 m m)) `inM` m

isAsymmetric :: Matrix Bool -> Bool
isAsymmetric m = force $ and $
  zipWith (\x y -> not (x == True && y == True)) (toList m) (toList (transpose m))

main :: IO ()
main = do
  as <- return $ filter isAsymmetric (force matrices5x5)
  putStrLn $ "Asymmetric: " ++ show (length as)
  putStrLn $ "Transitive + asymmetric: " ++ show (length (filter isTransitive as))