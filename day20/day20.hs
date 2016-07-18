import qualified Data.Vector.Unboxed as V

elfSieve1 :: Int -> V.Vector Int
elfSieve1 limit = V.accum (+) (V.replicate limit 0)
    [(n*k, 10*n) | n <- [1..limit-1], k <- [1..(limit-1) `div` n]]

elfSieve2 :: Int -> V.Vector Int
elfSieve2 limit = V.accum (+) (V.replicate limit 0)
    [(n*k, 11*n) | n <- [1..limit-1], k <- [1..min 50 ((limit-1)`div`n)]]

main :: IO ()
main = do
    let elf1 = elfSieve1 1000000
        elf2 = elfSieve2 1000000
        input = 34000000
    print $ V.findIndex (>= input) elf1
    print $ V.findIndex (>= input) elf2

{-
786240
831600
-}
