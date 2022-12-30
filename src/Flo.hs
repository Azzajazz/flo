module Flo (
    parseFlo,
    parse
) where

import Control.Applicative
import Data.Char
import Debug.Trace

{-
Flo grammar (to be expanded on)
Atoms: Ident, TypeName, IntLit

program := (funcsig | funcdef)*
funcsig := Ident : typelist;
typelist := TypeName -> typelist | TypeName
funcdef := Ident arglist = expr
arglist := Ident arglist | (empty)

expr := expr + expr'
expr' := Ident | IntLit | (expr)
-}

type Ident = String
type TypeName = String

data Expr = EPlus Expr' Expr | ESub Expr' 
    deriving Show

data Expr' = EIdent Ident | EIntLit Int | EGroup Expr
    deriving Show

data FuncInfo = FuncSig Ident [TypeName]
    | FuncDef Ident [Ident] Expr
    deriving Show

data AST = Prog [FuncInfo] deriving Show

data ParseError a = Unexpected a
    | UnexpectedEOF

instance Show a => Show (ParseError a) where
    show (Unexpected a) = "unexpected token: " ++ show a
    show UnexpectedEOF  = "unexpected end of file."

newtype Parser a b = Parser {runParser :: [a] -> Either (ParseError a) [(b, [a])]}

instance Functor (Parser a) where
    fmap f (Parser run) = Parser $ \s -> do
        options <- run s
        return $ map (\(x, y) -> (f x, y)) options

instance Applicative (Parser a) where
    pure x = Parser $ \s -> Right [(x, s)]
    Parser runF <*> Parser runX = Parser $ \s -> do
        (resultsX, restsX) <- unzip <$> runX s
        optionsF <- traverse runF restsX
        return $ concat $ zipWith (\t l -> [(f t, r) | (f, r) <- l]) resultsX optionsF

instance Monad (Parser a) where
    Parser run >>= f = Parser $ \s -> do 
        options <- run s
        optionsF <- reverse <$> traverse (\(t, r) -> runParser (f t) r) options 
        return $ concat optionsF

instance Alternative (Parser a) where
    empty = Parser $ \_ -> Left UnexpectedEOF
    Parser run1 <|> Parser run2 = Parser $ \s -> run1 s <> run2 s

parse :: Parser a b -> [a] -> Either (ParseError a) [(b, [a])]
parse p s = runParser p s

isTerminator :: Char -> Bool
isTerminator c = c `elem` ":;-=+()" || isSpace c

charP :: Char -> Parser Char Char
charP c = Parser $ \s -> if null s then
        Left $ UnexpectedEOF
    else if head s == c then
        Right $ [(c, tail s)]
    else
        Left $ Unexpected (head s)

stringP :: String -> Parser Char String
stringP = fmap reverse . traverse charP . reverse

tokenP :: Parser Char String
tokenP = Parser $ \s -> if null s then
        Left UnexpectedEOF
    else
        Right [break isTerminator s]

ignoreSpace :: Parser Char b -> Parser Char b       
ignoreSpace (Parser run) = Parser $ \s -> run (dropWhile isSpace s)

try :: Parser a b -> Parser a (Maybe b)
try (Parser run) = Parser $ \s -> case run s of
    Left _ -> Right [(Nothing, s)]
    Right options -> Right $ map (\(x, _) -> (Just x, s)) options

tryConsume :: Parser a b -> Parser a (Maybe b)
tryConsume (Parser run) = Parser $ \s -> case run s of
    Left _  -> Right [(Nothing, s)] 
    Right options -> Right $ map (\(x, y) -> (Just x, y)) options

eof :: Parser a () 
eof = Parser $ \s -> if null s then Right [((), s)] else Left (Unexpected $ head s)

parseEIdent :: Parser Char Expr'
parseEIdent = do
    ident <- tokenP
    return $ EIdent ident

parseEIntLit :: Parser Char Expr'
parseEIntLit = Parser $ \s -> let
    (pref, suff) = break isTerminator s
    (n, garbage) = span isDigit pref in
        if null s then
            Left $ UnexpectedEOF
        else if null garbage then
            Right [(EIntLit $ read n, suff)]
        else
            Left $ Unexpected (head garbage)

parseEGroup :: Parser Char Expr'
parseEGroup = do
    _ <- charP '('
    expr <- ignoreSpace $ parseExpr
    _ <- ignoreSpace $ charP ')'
    return $ EGroup expr

parseExpr' :: Parser Char Expr' 
parseExpr' = ignoreSpace $ parseEGroup <|> parseEIntLit <|> parseEIdent

parseESub :: Parser Char Expr
parseESub = do
    subexpr <- parseExpr'
    return $ ESub subexpr

parseEPlus :: Parser Char Expr
parseEPlus = do
    first <- parseExpr'
    _ <- ignoreSpace $ charP '+'
    second <- ignoreSpace $ parseExpr
    return $ EPlus first second

parseExpr :: Parser Char Expr
parseExpr = ignoreSpace $ parseEPlus <|> parseESub

parseTypeList :: Parser Char [TypeName]
parseTypeList = do
    check <- tryConsume $ charP ';'
    if check == Nothing then do
        _ <- tryConsume $ stringP "->"
        typename <- ignoreSpace $ tokenP
        typenames <- ignoreSpace $ parseTypeList
        return (typename:typenames)
    else do
        return []

parseFuncSig :: Parser Char FuncInfo
parseFuncSig = do 
    name <- tokenP
    _ <- ignoreSpace $ charP ':'
    typenames <- ignoreSpace parseTypeList
    return $ FuncSig name typenames

parseArgList :: Parser Char [Ident]
parseArgList = do
    check <- try $ charP '='
    if check == Nothing then do
        arg <- tokenP
        args <- ignoreSpace parseArgList
        return (arg:args)
    else do
        return []

parseFuncDef :: Parser Char FuncInfo
parseFuncDef = do
    name <- tokenP
    args <- ignoreSpace parseArgList
    _ <- ignoreSpace $ charP '='
    expr <- ignoreSpace parseExpr
    _ <- ignoreSpace $ charP ';'
    return $ FuncDef name args expr

parseFlo :: Parser Char AST
parseFlo = do
    attempt <- try eof
    if attempt == Nothing then do
        next <- ignoreSpace $ parseFuncSig <|> parseFuncDef
        Prog rest <- ignoreSpace $ parseFlo
        return $ Prog (next:rest)
    else do
        return $ Prog []

--TODO: Typechecking
--TODO: Semantics
--TODO: Emitting