import Data.List (transpose)

type Reindeer = (String, Int, Int, Int)

parse :: String -> [Reindeer]
parse = map (line . words) . lines
    where
        line [name,_,_,s,_,_,t,_,_,_,_,_,_,r,_] = (name, read s, read t, read r)

posBySecond :: Reindeer -> [Int]
posBySecond (_, speed, flyT, restT) = scanl1 (+) . cycle
    $ replicate flyT speed ++ replicate restT 0

stepScore :: [Int] -> [Int]
stepScore rds = [if rd == best then 1 else 0 | rd <- rds]
    where best = maximum rds

tally :: [[Int]] -> [Int]
tally = foldl1 (zipWith (+)) . map stepScore

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    let timeLimit = 2503
        race = take timeLimit . transpose $ map posBySecond input
    print $ maximum $ last race
    print $ maximum $ tally race

{-
2640
1102
-}
