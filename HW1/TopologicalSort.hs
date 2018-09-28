import Data.Matrix
import Data.Vector (elemIndex)
import Data.List (nub, union, delete, elem)

type Vertex = Int
type Edge   = (Vertex, Vertex)

allVertices :: [Vertex]
allVertices = [1,2..]

sort1 :: [Edge] -> [Vertex]
sort1 edges = sort1' edges vs []
  where
    vs = nub $ (fst <$> edges) `union` (snd <$> edges)

sort1' :: [Edge] -> [Vertex] -> [Vertex] -> [Vertex]
sort1' _     []        _    = []
sort1' edges (v:white) gray = sort1'' v edges white gray

sort1'' :: Vertex -> [Edge] -> [Vertex] -> [Vertex] -> [Vertex]
sort1'' v edges white gray = case lookup v edges of
  Nothing -> case gray of
    []     -> sort1' (filter (\(_,x) -> x /= v) edges) white [] ++ [v]
    (g:gs) -> sort1'' g (filter (\(_,x) -> x /= v) edges) white gs ++ [v]
  Just x  -> if x `elem` white
    then sort1'' x (delete (v,x) edges) (delete x white) (v:gray)
    else []

sort2 :: Matrix Bool -> [Vertex]
sort2 m = sort2' m vs []
  where vs = take (nrows m) allVertices

sort2' :: Matrix Bool -> [Vertex] -> [Vertex] -> [Vertex]
sort2' _ []        _    = []
sort2' m (v:white) gray = sort2'' v m white gray

sort2'' :: Vertex -> Matrix Bool -> [Vertex] -> [Vertex] -> [Vertex]
sort2'' v m white gray = case elemIndex True (getRow v m) of
  Nothing -> case gray of
    []     -> sort2' (clearCol v m) white [] ++ [v]
    (g:gs) -> sort2'' g (clearCol v m) white gs ++ [v]
  Just i  -> let x = i+1 in if x `elem` white
    then sort2'' x (setElem False (v,x) m) (delete x white) (v:gray)
    else []

clearCol :: Int -> Matrix Bool -> Matrix Bool
clearCol = mapCol (\_ _ -> False)

main :: IO ()
main = do
  print $ sort1 test1
  print $ sort2 test2

-- ================
-- ===== Test =====
-- ================

test1 :: [Edge]
test1 =
  [ (1, 2)
  , (1, 3)
  , (1, 4)
  , (1, 5)
  , (2, 4)
  , (3, 4)
  , (3, 5)
  , (4, 5)
  ]

test2 :: Matrix Bool
test2 = fromLists
  [ [ False, True,  True,  True,  True  ]
  , [ False, False, False, True,  False ]
  , [ False, False, False, True,  True  ]
  , [ False, False, False, False, True  ]
  , [ False, False, False, False, False ]
  ]
