import Control.Monad ((>=>))
import Data.List (group)

looksay :: [Char] -> [Char]
looksay = group >=> (++) . show . length <*> take 1

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
