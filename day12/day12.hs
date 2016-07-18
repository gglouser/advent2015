import Control.Applicative
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

data Json = Str String
          | Num Int
          | Array [Json]
          | Object (Map.Map String Json)
    deriving (Eq, Show)

parse :: String -> Either P.ParseError Json
parse = P.parse json ""
    where
        json = Str <$> str
           <|> Num <$> num
           <|> Array <$> arr
           <|> Object <$> obj
        str = P.between (P.char '"') (P.char '"') (many (P.noneOf "\""))
        num = P.option id (negate <$ P.char '-') <*> (read <$> some P.digit)
        arr = P.between (P.char '[') (P.char ']') (json `P.sepBy` P.char ',')
        obj = Map.fromList <$> P.between (P.char '{') (P.char '}') (slot `P.sepBy` P.char ',')
        slot = (,) <$> str <* P.char ':' <*> json

eval :: Json -> Int
eval (Str _) = 0
eval (Num n) = n
eval (Array a) = sum $ map eval a
eval (Object o) = sum $ Map.map eval o

dered :: Json -> Json
dered (Object o) = Object $ if elem (Str "red") (Map.elems o) then Map.empty else Map.map dered o
dered (Array a) = Array $ map dered a
dered v = v

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parse input of
        Left err -> print err
        Right ledger -> do
            print $ eval ledger
            print $ eval $ dered ledger

{-
156366
96852
-}
