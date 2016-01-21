import Control.Applicative
import qualified Text.Parsec as P

type Parser = P.Parsec String ()

pstring :: Parser Int
pstring = (+ 2) <$> P.between (P.char '"') (P.char '"') (sum <$> P.many pchar)
pchar = (P.char '\\' *> pesc) <|> 0 <$ P.noneOf "\\\""
pesc = 1 <$ P.char '\\'
    <|> 1 <$ P.char '"'
    <|> 3 <$ (P.char 'x' *> P.hexDigit *> P.hexDigit *> return '.')

unpstring :: Parser Int
unpstring = (+ 4) <$> P.between (P.char '"') (P.char '"') (sum <$> P.many unpchar)
unpchar = (P.char '\\' *> unpesc) <|> 0 <$ P.noneOf "\\\""
unpesc = 2 <$ P.char '\\'
    <|> 2 <$ P.char '"'
    <|> 1 <$ (P.char 'x' *> P.hexDigit *> P.hexDigit *> return '.')

main = do
    print "hello world"
    input <- readFile "input.txt"
    case P.parse (pstring `P.endBy` P.spaces) "input.txt" input of
        Left err -> print err
        Right ns -> putStrLn $ "shrinking: " ++ show (sum ns)
    case P.parse (unpstring `P.endBy` P.spaces) "input.txt" input of
        Left err -> print err
        Right ns -> putStrLn $ "expanding: " ++ show (sum ns)

{-
shrinking: 1371
expanding: 2117
-}
