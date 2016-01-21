import Data.List
import Data.Ord

type Ingredient = [Int]

parse :: String -> [Ingredient]
parse = map (map read . pline . words) . lines
    where pline l = [init (l!!2), init (l!!4), init (l!!6), init (l!!8), l!!10]

iscale vs n = map (* n) vs

score :: [Ingredient] -> [Int] -> (Int, Int)
score is ns = (product . map (max 0) $ init xs, last xs)
    where xs = foldl1 (zipWith (+)) $ zipWith iscale is ns

gen :: Int -> Int -> [[Int]]
gen _ 0 = [[]]
gen tsps 1 = [[tsps]]
gen tsps n = [tsp : xs | tsp <- [0..tsps], xs <- gen (tsps-tsp) (n-1)]

scores is tsps = [(ts, score is ts) | ts <- gen tsps (length is)]

main = do
    input <- parse `fmap` readFile "input.txt"
    putStrLn $ show input
    let s = scores input 100
    print $ length s
    print $ maximumBy (comparing (fst . snd)) $ s
    print $ maximumBy (comparing (fst . snd)) . filter ((== 500) . (snd . snd)) $ s

{-
222870
117936
-}
