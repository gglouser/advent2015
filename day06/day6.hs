import Control.Applicative
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Text.Parsec as P

type Pos = (Int,Int)
data Command = TurnOn | TurnOff | Toggle deriving Show
data Action = Action Command Pos Pos deriving Show

gridSize = 1000*1000
allOff = V.replicate gridSize False
allOn  = V.replicate gridSize True

indices :: Int -> Int -> Int -> Int -> V.Vector Int
indices x0 y0 x1 y1 = V.fromList [1000*y + x | y <- [y0..y1], x <- [x0..x1]]

eval :: V.Vector Bool -> Action -> V.Vector Bool
eval v (Action TurnOn  (x0,y0) (x1,y1)) = V.update_ v (indices x0 y0 x1 y1) allOn
eval v (Action TurnOff (x0,y0) (x1,y1)) = V.update_ v (indices x0 y0 x1 y1) allOff
eval v (Action Toggle  (x0,y0) (x1,y1)) = V.accumulate_ (==) v (indices x0 y0 x1 y1) allOff

eval2 :: V.Vector Int -> Action -> V.Vector Int
eval2 v (Action TurnOn  (x0,y0) (x1,y1)) = V.accumulate_ (+) v (indices x0 y0 x1 y1) all1
eval2 v (Action TurnOff (x0,y0) (x1,y1)) = V.accumulate_ (decr) v (indices x0 y0 x1 y1) all1
eval2 v (Action Toggle  (x0,y0) (x1,y1)) = V.accumulate_ (+) v (indices x0 y0 x1 y1) all2

decr 0 _ = 0
decr n _ = n-1

all0 = V.replicate gridSize 0 :: V.Vector Int
all1 = V.replicate gridSize 1
all2 = V.replicate gridSize 2

pNum = read <$> P.many1 P.digit
pPos = (,) <$> pNum <* P.char ',' <*> pNum
pCommand = TurnOn  <$ P.try (P.string "turn on ")
       <|> TurnOff <$ P.try (P.string "turn off ")
       <|> Toggle  <$ P.string "toggle "
pAction = Action <$> pCommand <*> pPos <* P.string " through " <*> pPos
pActions = pAction `P.endBy` P.endOfLine

main = do
    input <- readFile "input.txt"
    case P.parse pActions "input.txt" input of
        Left err -> print err
        Right acts -> do
            let r = foldl' eval allOff acts
            print $ V.length . V.filter id $ r
            let r2 = foldl' eval2 all0 acts
            print $ V.sum $ r2

{-
400410
15343601
-}
