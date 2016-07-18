import qualified Data.ByteString.Char8 as C

paperReq :: (Int, Int, Int) -> Int
paperReq (a,b,c) = 2*sum areas + minimum areas
    where areas = [a*b, b*c, c*a]

ribbonReq :: (Int, Int, Int) -> Int
ribbonReq (a,b,c) = 2*minimum [a+b, a+c, b+c] + a*b*c

parse :: C.ByteString -> [(Int, Int, Int)]
parse = map (tup . map toI . C.split 'x') . C.lines
    where
        tup (a:b:c:_) = (a,b,c)
        toI = maybe (error "expected number") fst . C.readInt

main :: IO ()
main = do
    input <- parse <$> C.readFile "input.txt"
    putStrLn $ "paper:  " ++ show (sum $ map paperReq input)
    putStrLn $ "ribbon: " ++ show (sum $ map ribbonReq input)

{-
paper:  1606483
ribbon: 3842356
-}
