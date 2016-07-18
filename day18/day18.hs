import qualified Data.Vector.Unboxed as V

data Grid = Grid Int Int (V.Vector Bool) deriving Show

parse :: String -> Grid
parse s = Grid w h $ V.fromList (rows >>= map (== '#'))
    where
        rows = lines s
        w = length rows
        h = length $ head rows

showGrid :: Grid -> String
showGrid g = unlines [[showLight (g ! (x,y)) | x <- [0..w-1]] | y <- [0..h-1]]
    where
        showLight True = '#'
        showLight False = '.'
        Grid w h _ = g

(!) :: Grid -> (Int, Int) -> Bool
(Grid w h v) ! (x,y)
    | x < 0 || x >= w || y < 0 || y >= h = False
    | otherwise = v V.! (x + w*y)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]

stepGrid :: (Grid -> (Int, Int) -> Bool) -> Grid -> Grid
stepGrid next g = Grid w h v
    where
        Grid w h _ = g
        v = V.fromList [next g (x,y) | y <- [0..h-1], x <- [0..w-1]]

nextState :: Grid -> (Int, Int) -> Bool
nextState g p = alive == 3 || g ! p && alive == 2
    where
        alive = length . filter (g !) $ neighbors p

isCorner :: Grid -> (Int, Int) -> Bool
isCorner (Grid w h _) (x,y) = (x == 0 || x == w-1) && (y == 0 || y == h-1)

nextState2 :: Grid -> (Int, Int) -> Bool
nextState2 g p = isCorner g p || nextState g p

turnCornersOn :: Grid -> (Int, Int) -> Bool
turnCornersOn g p = isCorner g p || g ! p

onCount :: Grid -> Int
onCount (Grid _ _ v) = V.length $ V.filter id v

main :: IO ()
main = do
    init <- parse <$> readFile "input.txt"
    let final1 = iterate (stepGrid nextState) init !! 100
    print $ onCount final1
    let init2 = stepGrid turnCornersOn init
        final2 = iterate (stepGrid nextState2) init2 !! 100
    print $ onCount final2

{-
821
886
-}
