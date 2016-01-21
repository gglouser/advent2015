import qualified Data.Vector.Unboxed as V

parse :: String -> [Bool]
parse = concatMap (map (== '#')) . lines

data Grid = Grid Int Int (V.Vector Bool) deriving Show

mkGrid :: Int -> Int -> [Bool] -> Grid
mkGrid w h init = Grid w h $ V.fromList init

showGrid g@(Grid w h v) = unlines $ map showRow [0..h-1]
    where
        showRow y = [if g ! (x,y) then '#' else '.' | x <- [0..w-1]]

(!) :: Grid -> (Int, Int) -> Bool
(Grid w h v) ! (x,y) | x < 0 || x >= w || y < 0 || y >= h = False
                     | otherwise                          = v V.! (y*w + x)

nextState :: Grid -> (Int, Int) -> Bool
nextState g (x,y)
    | g ! (x,y) = alive == 3 || alive == 4
    | otherwise = alive == 3
    where alive = length $ filter id [g!(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1]]

nextState2 g@(Grid w h _) (x,y)
    | (x == 0 || x == w-1) && (y == 0 || y == h-1) = True
    | otherwise = nextState g (x,y)

stepGrid next g@(Grid w h _) = mkGrid w h $ [next g (x,y) | y <- [0..h-1], x <- [0..w-1]]

onCount (Grid _ _ v) = V.length $ V.filter id v

turnCornersOn g@(Grid w h _) (x,y)
    | (x == 0 || x == w-1) && (y == 0 || y == h-1) = True
    | otherwise = g ! (x,y)

main = do
    input <- parse `fmap` readFile "input.txt"
    let init = mkGrid 100 100 input
        final1 = iterate (stepGrid nextState) init !! 100
    print $ onCount final1
    let init2 = stepGrid turnCornersOn init
        final2 = iterate (stepGrid nextState2) init2 !! 100
    print $ onCount final2
    -- let test = parse ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####..\n"
        -- testG = mkGrid 6 6 test
    -- putStrLn $ showGrid testG
    -- print "------"
    -- putStrLn $ showGrid . stepGrid $ testG
    -- print "------"
    -- putStrLn $ showGrid . stepGrid . stepGrid $ testG

{-
821
886
-}
