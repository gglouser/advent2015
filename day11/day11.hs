pwIncr :: [Char] -> [Char]
pwIncr [] = []
pwIncr ('z':cs) = 'a' : pwIncr cs
pwIncr ('h':cs) = 'j' : cs          -- skip 'i'
pwIncr ('k':cs) = 'm' : cs          -- skip 'l'
pwIncr ('n':cs) = 'p' : cs          -- skip 'o'
pwIncr (c:cs) = succ c : cs

pwFindInit :: [Char] -> [Char]
pwFindInit = g . foldr f ([], False)
    where
        f 'i' (s, False) = ('j':s, True)
        f 'l' (s, False) = ('m':s, True)
        f 'o' (s, False) = ('p':s, True)
        f c (s, False) = (c:s, False)
        f _ (s, True) = ('a':s, True)

        g (pw, True) = pw
        g (pw, False) = pwIncr pw

checkStraight :: [Char] -> Bool
checkStraight = or . (zipWith3 (\a b c -> a == succ b && b == succ c) <*> drop 1 <*> drop 2)

nPair :: Eq a => Int -> [a] -> Bool
nPair 0 _ = True
nPair n (a:s@(b:s')) = a == b && nPair (n-1) s' || nPair n s
nPair _ _ = False

valid :: [Char] -> Bool
valid pw = checkStraight pw && nPair 2 pw

pwFind :: [Char] -> [[Char]]
pwFind = map reverse . filter valid . iterate pwIncr . pwFindInit . reverse

input :: [Char]
input = "vzbxkghb"

main :: IO ()
main = putStr . unlines . take 2 $ pwFind input

{-
vzbxxyzz
vzcaabcc
-}
