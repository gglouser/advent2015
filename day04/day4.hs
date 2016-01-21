import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B

check5 bs = B.index bs 0 == '\0'
         && B.index bs 1 == '\0'
         && B.index bs 2 < '\x10'

check6 bs = B.index bs 0 == '\0'
         && B.index bs 1 == '\0'
         && B.index bs 2 == '\0'

search key checker = head $ filter check' [0..]
    where
        check' = checker . hash . B.append (B.pack key) . B.pack . show

main = do
    let key = "ckczppom"
    putStrLn $ "five zeroes: " ++ show (search key check5)
    putStrLn $ "six zeroes:  " ++ show (search key check6)

{-
five zeroes: 117946
six zeroes:  3938038
-}
