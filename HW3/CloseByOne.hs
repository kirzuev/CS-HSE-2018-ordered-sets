import Data.Matrix hiding (toList)
import Data.Vector (toList)
import Data.List (intersect, union, (\\), nub)

inputMatrix :: Matrix Bool
inputMatrix = fromLists
  --    a      b      c      d      e
  [ [  True, False, False, False, False ] -- 1
  , [  True,  True, False,  True, False ] -- 2
  , [  True,  True,  True,  True, False ] -- 3
  , [ False, False,  True, False,  True ] -- 4
  , [ False,  True, False,  True, False ] -- 5
  , [ False, False,  True,  True,  True ] -- 6
  ]

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

showFCList :: [([Int], [Int])] -> String
showFCList []         = ""
showFCList ((a,b):xs) = "A = " ++ show a ++ ", B = " ++ (numToLetter <$> b) ++ "\n" ++ showFCList xs

numToLetter :: Int -> Char
numToLetter i = snd $ head $
  filter (\(n,_) -> n == i) (zip [1,2..] ['a','b'..])

closeByOne :: Matrix Bool -> [([Int], [Int])]
closeByOne m = foldr union [] $
  (\g -> process m [g] g ((up m [g]), (down m $ up m [g]))) <$> [1..nrows m]

process :: Matrix Bool -> [Int] -> Int -> ([Int], [Int]) -> [([Int], [Int])]
process m a g (c, d) = (if hs == [] then [(c, d)] else [])
  ++ (foldr union [] $ (getMinGen m (c, d) <$> fs))
  where
    hs = takeWhile  (< g) (c \\ a)
    fs = dropWhile (<= g) ([1..nrows m] \\ c)

getMinGen :: Matrix Bool -> ([Int], [Int]) -> Int -> [([Int], [Int])]
getMinGen m (c, d) f = process m z f (x, y)
  where
    z = c `union` [f]
    y = d `intersect` (up m [f])
    x = down m y

main :: IO ()
main = do
  let formalConcepts = closeByOne inputMatrix
  putStrLn $ showFCList formalConcepts
  putStrLn $ "Number of pairs = " ++ show (length formalConcepts)