{-# LANGUAGE NoImplicitPrelude #-}
import BasePrelude
key = "3113322113"
nth n = length $ iterate (group >=> (++) . show . length <*> take 1) key !! n
main = print (nth 40, nth 50)
