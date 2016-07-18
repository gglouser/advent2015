newtype Code = Code { getCode :: Int } deriving Show

toCode :: Int -> Code
toCode = Code . (`mod` 33554393)

instance Num Code where
    (Code a) + (Code b) = toCode $ a + b
    (Code a) * (Code b) = toCode $ a * b
    abs (Code a) = toCode $ abs a
    signum (Code 0) = 0
    signum _ = 1
    negate (Code a) = toCode $ negate a
    fromInteger n = toCode $ fromInteger n

coordToIndex :: Int -> Int -> Int
coordToIndex row col = (x*(x-1)) `div` 2 + col
    where x = row + col - 1

main :: IO ()
main = do
    let i = coordToIndex 2947 3029
    print $ getCode $ 20151125 * 252533^(i-1)

{-
19980801
-}
