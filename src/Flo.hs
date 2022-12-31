module Flo (
    parse,
    progP
) where

import Control.Applicative
import Data.Char

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

data Expr = EPlus Var Expr
    | EVar Var
    deriving Show

data Var = VIntLit Int
    | VIdent Ident
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
    empty = errEofP
    Parser run1 <|> Parser run2 = Parser $ \s -> run1 s <> run2 s

parse :: Parser a b -> [a] -> Either (ParseError a) [(b, [a])]
parse p s = runParser p s

errEofP :: Parser a b
errEofP = Parser $ \_ -> Left UnexpectedEOF

satP :: (a -> Bool) -> Parser a a
satP p = Parser $ \s -> case s of
    [] -> Left UnexpectedEOF
    x:xs
        | p x       -> Right [(x, xs)]
        | otherwise -> Left $ Unexpected x

charP :: Char -> Parser Char Char
charP c = satP (==c)

digitP :: Parser Char Char
digitP = satP isDigit

stringP :: String -> Parser Char String
stringP []     = return []
stringP (x:xs) = charP x >> stringP xs

manyC :: Parser a b -> Parser a [b]
manyC parser = do {
        next <- parser; 
        rest <- manyC parser;
        return (next:rest);
    } <|> return []

many1C :: Parser a b -> Parser a [b]
many1C parser = do
    next <- parser
    rest <- manyC parser
    return (next:rest)

noSpaceC :: Parser Char b -> Parser Char b
noSpaceC (Parser run) = Parser $ run . dropWhile isSpace

sepByC :: Parser a c -> Parser a b -> Parser a [b]
sepByC sep parser = do
    first <- parser
    rest <- manyC (sep >> parser)
    return (first:rest)

specials :: [Char]
specials = ";:+=-"

tokenP :: Parser Char String
tokenP = many1C $ satP (\c -> not $ isTerminator c)
    where
        isTerminator c = c `elem` specials || isSpace c

identP :: Parser Char String
identP = noSpaceC tokenP

typeP :: Parser Char String
typeP = noSpaceC tokenP

intLitP :: Parser Char String
intLitP = noSpaceC $ many1C digitP

vIntLitP :: Parser Char Var
vIntLitP = intLitP >>= return . VIntLit . read

vIdentP :: Parser Char Var
vIdentP = identP >>= return . VIdent

varP :: Parser Char Var
varP = vIntLitP <|> vIdentP

eVarP :: Parser Char Expr
eVarP = varP >>= return . EVar 

ePlusP :: Parser Char Expr
ePlusP = do
    var <- varP
    _ <- noSpaceC $ charP '+'
    expr <- exprP
    return $ EPlus var expr

exprP :: Parser Char Expr
exprP = ePlusP <|> eVarP

funcSigP :: Parser Char FuncInfo
funcSigP = do
    name <- identP
    _ <- noSpaceC $ charP ':'
    types <- sepByC (noSpaceC $ stringP "->") typeP
    return $ FuncSig name types

funcDefP :: Parser Char FuncInfo
funcDefP = do
    name <- identP
    args <- manyC identP
    _ <- noSpaceC $ charP '='
    expr <- exprP
    return $ FuncDef name args expr

progP :: Parser Char AST
progP = many1C (funcDefP <|> funcSigP) >>= return . Prog
