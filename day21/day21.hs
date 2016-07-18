import Data.List (minimumBy, maximumBy, partition)
import Data.Ord (comparing)

consume3 :: (a -> a -> a -> b) -> [a] -> b
consume3 f (a:b:c:_) = f a b c

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = []
choose n (i:is) = map (i:) (choose (n-1) is) ++ choose n is

divCeil :: Integral a => a -> a -> a
divCeil i j = (i + j - 1) `div` j

-----

data Object = Obj { hp, damage, armor :: Int } deriving Show
type ShopItem = (Int, Object)

parseBoss :: String -> Object
parseBoss = consume3 Obj . map (read . last . words) . lines

parseShop :: String -> ([ShopItem], [ShopItem], [ShopItem])
parseShop s = (map item weaps, map item armors, map item rings)
    where
        item = consume3 mk . map read . words . drop 12
        mk cost d a = (cost, Obj 0 d a)
        (weaps, s') = break null . drop 1 $ lines s
        (armors, s'') = break null $ drop 2 s'
        rings = drop 2 s''

-- Turns To Kill
ttk :: Int -> Int -> Int -> Int
ttk dmg hp arm = hp `divCeil` max 1 (dmg - arm)

-- Resolve combat - return true if attacker wins
combat :: Object -> Object -> Bool
combat (Obj ahp admg aarm) (Obj dhp ddmg darm) = ttk ddmg ahp aarm >= ttk admg dhp darm

loadouts :: [ShopItem] -> [ShopItem] -> [ShopItem] -> [(Int, [Object])]
loadouts weaps arms rings = do
    w <- weaps
    a <- [0,1] >>= flip choose arms
    r <- [0..2] >>= flip choose rings
    return . foldr (\(c,o) (t,os) -> (c+t, o:os)) (0,[]) $ w : a ++ r

equip :: Object -> [Object] -> Object
equip = foldl oadd
    where
        oadd (Obj hp1 d1 a1) (Obj hp2 d2 a2) = Obj (hp1+hp2) (d1+d2) (a1+a2)

main :: IO ()
main = do
    boss <- parseBoss `fmap` readFile "input.txt"
    itemShop <- parseShop `fmap` readFile "itemshop.txt"
    let options = uncurry3 loadouts itemShop
        player = Obj 100 0 0
        playerWins items = combat (equip player items) boss
        (wins, losses) = partition (playerWins . snd) options
    print $ minimumBy (comparing fst) wins
    print $ maximumBy (comparing fst) losses

{-
121
201
-}
