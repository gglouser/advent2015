import qualified Data.Set as Set

move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) '^' = (x, y+1)
move (x,y) 'v' = (x, y-1)
move (x,y) '>' = (x+1, y)
move (x,y) '<' = (x-1, y)
move (x,y) _   = (x, y)

track :: [Char] -> [(Int, Int)]
track = scanl move (0,0)

tallyHouses :: [(Int, Int)] -> Int
tallyHouses = Set.size . Set.fromList

fork :: [a] -> ([a], [a])
fork = foldr (\x (a,b) -> (x:b,a)) ([],[])

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "year 1: " ++ show (tallyHouses $ track input)
    let (santa, robot) = fork input
    putStrLn $ "year 2: " ++ show (tallyHouses $ track santa ++ track robot)

{-
year 1: 2572
year 2: 2631
-}
