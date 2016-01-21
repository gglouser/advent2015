paperReq (a,b,c) = 2*(x + y + z) + min x (min y z)
    where
        x = a*b
        y = b*c
        z = c*a

ribbonReq (a,b,c) = 2*(min (a+b) (min (a+c) (b+c))) + a*b*c

reader :: String -> (Int, Int, Int)
reader s = (read a, read b, read c)
    where
        (a,s') = break (== 'x') s
        (b,s'') = break (== 'x') $ tail s'
        c = tail s''

main = do
    input <- (map reader . lines) `fmap` readFile "input.txt"
    putStrLn $ "paper:  " ++ show (sum $ map paperReq input)
    putStrLn $ "ribbon: " ++ show (sum $ map ribbonReq input)

{-
paper:  1606483
ribbon: 3842356
-}
