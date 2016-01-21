undigs3 = foldl (\a k -> 3*a + k) 0
firstNum = undigs3 [1,1,1,0,2,0,2,2,0]
secondNum = undigs3 [1,0,2,2,1,1,1,2,2,0,1]

naiveCollatz 1 b = b
naiveCollatz a b = naiveCollatz a' $! (b+1)
    where a' = if even a then a `div` 2 else 3*a+1

main = do
    print $ naiveCollatz firstNum 0
    print $ naiveCollatz secondNum 0

{-
184
231
-}
