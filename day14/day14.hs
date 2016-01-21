import Data.List

type Reindeer = (String, Int, Int, Int)

parse :: String -> [Reindeer]
parse = map (line . words) . lines
    where line s = (s!!0, read (s!!3), read (s!!6), read (s!!13))

dist :: Int -> Reindeer -> Int
dist t (_, speed, flyT, restT) = (t1*flyT + min flyT t2) * speed
    where (t1, t2) = t `divMod` (flyT + restT)

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
    input <- parse `fmap` readFile "input.txt"
    let timeLimit = 2503
        race = take timeLimit . transpose $ map posBySecond input
        getName (name,_,_,_) = name
    print $ zip3 (map getName input) (last race) (tally race)
    print $ maximum $ last race
    print $ maximum $ tally race

{-
2640
1102
-}
