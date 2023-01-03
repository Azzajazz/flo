module Flo (
    parse,
    progP
) where

import Control.Applicative
import Data.Char

{-
-------- GRAMMAR --------
program := (funcSignature | funcDefinition)+
funcSignature := ident : type
funcDefinition := ident (ident)* = expr
expr := (literal | ident) + expr | literal | ident
type := (Int | Bool) (-> type)*
-}

type Ident = String
data Type = TBool
    | TInt
    | TFunc Type Type
    deriving Show

data Expr = EPlus (Maybe Type) Var Expr
    | EVar (Maybe Type) Var
    deriving Show

data Var = VIntLit Int
    | VIdent (Maybe Type) Ident
    deriving Show

data FuncInfo = FuncSig Ident Type
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

tIntP :: Parser Char Type
tIntP = noSpaceC (stringP "Int") >> return TInt

tBoolP :: Parser Char Type
tBoolP = noSpaceC (stringP "Bool") >> return TBool

litTypeP :: Parser Char Type 
litTypeP = tIntP <|> tBoolP

funcTypeP :: Parser Char Type
funcTypeP = do
    first <- litTypeP
    rest <- noSpaceC (stringP "->") >> (noSpaceC funcTypeP <|> litTypeP)
    return $ TFunc first rest


intLitP :: Parser Char String
intLitP = noSpaceC $ many1C digitP

vIntLitP :: Parser Char Var
vIntLitP = intLitP >>= return . VIntLit . read

vIdentP :: Parser Char Var
vIdentP = identP >>= return . VIdent Nothing

varP :: Parser Char Var
varP = vIntLitP <|> vIdentP

eVarP :: Parser Char Expr
eVarP = varP >>= return . EVar Nothing 

ePlusP :: Parser Char Expr
ePlusP = do
    var <- varP
    _ <- noSpaceC $ charP '+'
    expr <- exprP
    return $ EPlus Nothing var expr

exprP :: Parser Char Expr
exprP = ePlusP <|> eVarP

funcSigP :: Parser Char FuncInfo
funcSigP = do
    name <- identP
    _ <- noSpaceC $ charP ':'
    functype <- funcTypeP
    return $ FuncSig name functype 

funcDefP :: Parser Char FuncInfo
funcDefP = do
    name <- identP
    args <- manyC identP
    _ <- noSpaceC $ charP '='
    expr <- exprP
    return $ FuncDef name args expr

progP :: Parser Char AST
progP = many1C (funcDefP <|> funcSigP) >>= return . Prog

-- TODO (BIG): Type checking
newtype TypeEnv = TypeEnv {getType :: Ident -> Maybe Type}

teEmpty :: TypeEnv
teEmpty = TypeEnv $ \_ -> Nothing

addType :: (Ident, Type) -> TypeEnv -> TypeEnv
addType (ident, typename) (TypeEnv lookupType) = TypeEnv newLookupType
    where
        newLookupType idt
            | idt == ident = Just typename
            | otherwise   = lookupType idt

removeType :: Ident -> TypeEnv -> TypeEnv
removeType ident (TypeEnv lookupType) = TypeEnv newLookupType
    where
        newLookupType idt
            | idt == ident = Nothing
            | otherwise    = lookupType idt

data TypeCheckError = TypeMismatch Type Type

instance Show TypeCheckError where
    show (TypeMismatch found expected) = "Types do not match. Expected " ++ show expected ++ ", found " ++ show found

type TypeChecker a = TypeEnv -> a -> Either TypeCheckError (TypeEnv, Type)

typeCheckProgram :: TypeChecker AST
typeCheckProgram (Prog infos) = undefined 

