type Ingredient = [Int]

parse :: String -> [Ingredient]
parse = map (map read . line . words) . lines
    where
        line [_,_,a,_,b,_,c,_,d,_,e] = [init a, init b, init c, init d, e]

iscale :: [Int] -> Int -> [Int]
iscale vs n = map (* n) vs

score :: [Ingredient] -> [Int] -> (Int, Int)
score is ns = (product . map (max 0) $ init xs, last xs)
    where xs = foldl1 (zipWith (+)) $ zipWith iscale is ns

gen :: Int -> Int -> [[Int]]
gen _ 0 = [[]]
gen tsps 1 = [[tsps]]
gen tsps n = [tsp : xs | tsp <- [0..tsps], xs <- gen (tsps-tsp) (n-1)]

scores :: [Ingredient] -> Int -> [([Int], (Int, Int))]
scores is tsps = [(ts, score is ts) | ts <- gen tsps (length is)]

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    let s = scores input 100
        finalScore = fst . snd
        cals = snd . snd
    print $ maximum . map finalScore $ s
    print $ maximum . map finalScore . filter ((== 500) . cals) $ s

{-
222870
117936
-}
