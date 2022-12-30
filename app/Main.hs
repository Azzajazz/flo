module Main (main) where

import Flo

main :: IO ()
main = do
    code <- readFile "simple.fl"
    let result = parse parseFlo code 
    print result