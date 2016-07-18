import Data.List (sortBy)
import Data.Ord (comparing)

subsetSum :: Int -> [Int] -> [[Int]]
subsetSum 0 _ = [[]]
subsetSum _ [] = []
subsetSum n (c:cs) = (if n >= c then map (c:) (subsetSum (n-c) cs) else [])
                   ++ subsetSum n cs

regroup :: Int -> [Int] -> [[Int]]
regroup n ws = subsetSum (sum ws `div` n) ws

ord :: [Int] -> [Int] -> Ordering
ord = comparing length `mappend` comparing product

main :: IO ()
main = do
    input <- map read . lines <$> readFile "input.txt"
    let opts = regroup 3 input
        firstGroup = head $ sortBy ord opts
    print $ product firstGroup
    let opts2 = regroup 4 input
        firstGroup2 = head $ sortBy ord opts2
    print $ product firstGroup2

{-
11266889531
77387711
-}
