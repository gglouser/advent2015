import Crypto.Hash (hash, Digest, MD5)
import qualified Data.ByteString.Char8 as C

checkZeros :: Show a => Int -> a -> Bool
checkZeros n = all (== '0') . take n . show

search :: String -> (Digest MD5 -> Bool) -> Int
search key check = head $ filter check' [0..]
    where
        check' = check . hash . C.append key' . C.pack . show
        key' = C.pack key

main :: IO ()
main = do
    let key = "ckczppom"
    putStrLn $ "five zeroes: " ++ show (search key (checkZeros 5))
    putStrLn $ "six zeroes:  " ++ show (search key (checkZeros 6))

{-
five zeroes: 117946
six zeroes:  3938038
-}
