{-# LANGUAGE NoImplicitPrelude #-}
import BasePrelude
import Text.Parsec (between, char, sepBy, digit, noneOf, parse, option)
import qualified Data.Map.Strict as Map

data Val = VS String | VN Int | VA [Val] | VO (Map.Map String Val) deriving (Show, Eq)

pnum = option id (negate <$ char '-') <*> (read <$> some digit)
pstr = between (char '"') (char '"') (many (noneOf "\""))
parr = between (char '[') (char ']') (pval `sepBy` char ',')
pslot = (,) <$> (pstr <* char ':') <*> pval
pobj = Map.fromList <$> between (char '{') (char '}') (pslot `sepBy` char ',')
pval = VS <$> pstr <|> VN <$> pnum <|> VA <$> parr <|> VO <$> pobj

eval :: Val -> Int
eval (VS _) = 0
eval (VN n) = n
eval (VA va) = sum $ map eval va
eval (VO vo) = Map.foldr ((+) . eval) 0 vo

dered :: Val -> Val
dered (VO vo) = VO $ if elem (VS "red") (Map.elems vo) then Map.empty else Map.map dered vo
dered (VA va) = VA $ map dered va
dered v = v

main = do
    input <- readFile "input.txt"
    case parse pval "input.txt" input of
        Left err -> print err
        Right ledger -> do
            print $ eval ledger
            print $ eval . dered $ ledger

{-
156366
96852
-}
