module Flo () where

import Control.Applicative
import Data.Char
import Data.List

import Utils (mapFst)

newtype Parser a b = Parser {runParser :: [a] -> [(b, [a])]}

parseFull :: Parser a b -> [a] -> [b]
parseFull parser input = map fst $ filter (null . snd) (runParser parser input)

instance Functor (Parser a) where
    fmap f (Parser run) = Parser $ \s -> mapFst f (run s)

instance Applicative (Parser a) where
    pure x = Parser $ \s -> [(x, s)]
    Parser runF <*> Parser runX = Parser $ \s ->
        [(f t, r') |(f, r) <- runF s, (t, r') <- runX r]

instance Monad (Parser a) where
    Parser run >>= f = Parser $ \s ->
        [(t', r') | (t, r) <- run s, (t', r') <- runParser (f t) r]

instance Alternative (Parser a) where
    empty = Parser $ \_ -> []
    Parser runX <|> Parser runY = Parser $ \s -> runX s ++ runY s

type Ident = String

data Prog = Prog [Decl] deriving Show

data Decl = Def Ident [Ident] Expr deriving Show

data Expr = Plus Unit Expr
    | Unit Unit
    deriving Show

data Unit = Apply Ident [Ident]
    | IntLit Int
    | Var Ident
    deriving Show

specials :: String
specials = ":-+="

sat :: (a -> Bool) -> Parser a a
sat p = Parser $ \s -> case s of
    [] -> []
    (c:cs)
        | p c -> [(c, cs)]
        | otherwise -> []

char :: Char -> Parser Char Char
char c = sat (== c)

digit :: Parser Char Char
digit = sat isDigit

string :: String -> Parser Char String
string s = reverse <$> traverse char (reverse s)

noSpace :: Parser Char b -> Parser Char b
noSpace (Parser run) = Parser $ run . dropWhile isSpace

ident :: Parser Char String
ident = noSpace $ some
    $ sat (\c -> c `notElem` specials && not (isSpace c))

parseVar :: Parser Char Unit
parseVar = Var <$> ident

parseApply :: Parser Char Unit
parseApply = Apply <$> ident <*> some ident

parseIntLit :: Parser Char Unit
parseIntLit = (IntLit . read) <$> noSpace (some digit)

parseUnit :: Parser Char Unit
parseUnit = parseApply <|> parseIntLit <|> parseVar

parseExprUnit :: Parser Char Expr
parseExprUnit = Unit <$> parseUnit

parsePlus :: Parser Char Expr
parsePlus = Plus <$> parseUnit <* noSpace (char '+') <*> parseExpr

parseExpr :: Parser Char Expr
parseExpr = parsePlus <|> parseExprUnit

parseDecl :: Parser Char Decl
parseDecl = Def <$> ident <*> some ident <* noSpace (char '=') <*> parseExpr

parseProg :: Parser Char Prog
parseProg = Prog <$> some parseDecl