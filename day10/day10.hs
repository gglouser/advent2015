import Data.List

looksay :: [Char] -> [Char]
looksay = concatMap (\xs -> show (length xs) ++ [head xs]) . group

lsLen :: Int -> [Char] -> Int
lsLen n = length . (!! n) . iterate looksay

main :: IO ()
main = do
    let input = "3113322113"
    print $ lsLen 40 input
    print $ lsLen 50 input

{-
329356
4666278
-}
