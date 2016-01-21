import Data.List

chooseW 0 _ = [[]]
chooseW w [] = []
chooseW w (x:xs) = takeX ++ chooseW w xs
    where
        takeX = if x <= w then [x:r | r <- chooseW (w-x) xs] else []

regroup n ws = chooseW (sum ws `div` n) ws

superChooseW :: Int -> [Int] -> [([Int], [Int])]
superChooseW 0 v = [([],v)]
superChooseW w [] = []
superChooseW w (x:xs) = takeX ++ [(u,x:v) | (u,v) <- superChooseW w xs]
    where takeX = if x <= w then [(x:u,v) | (u,v) <- superChooseW (w-x) xs] else []

superGroup :: Int -> [Int] -> [[[Int]]]
superGroup 0 _ = []
superGroup 1 ws = [[ws]]
superGroup n ws = [u:g | (u,v) <- superChooseW (sum ws `div` n) ws, g <- superGroup (n-1) v]

ord xs ys = case compare (length xs) (length ys) of
                EQ -> compare (product xs) (product ys)
                c -> c

main = do
    input <- map read . lines <$> readFile "input.txt"
    print $ (input :: [Int])
    print $ sum input
    let opts = regroup 3 input
        firstGroup = head $ sortBy ord opts
    print $ length opts
    print firstGroup
    print $ product firstGroup
    -----
    let opts2 = regroup 4 input
        firstGroup2 = head $ sortBy ord opts2
    print $ length opts2
    print firstGroup2
    print $ product firstGroup2
    -----
    print $ length $ superGroup 4 input

{-
11266889531
77387711
-}
