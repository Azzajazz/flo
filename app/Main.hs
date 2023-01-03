module Main (main) where

import Flo

main :: IO ()
main = do
    code <- readFile "simple.fl"
    let result = parse progP code
    case result of
        Left e -> print e
        Right ((ast, _):_) -> do
            let typecheck = typeCheckProg ast
            print ast 
            print typecheck
        _ -> error "unreachable"