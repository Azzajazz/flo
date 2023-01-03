module Flo (
    parse,
    progP,
    typeCheckProg
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

data Expr = EPlus Var Expr
    | EVar Var
    deriving Show

data Var = VIntLit Int
    | VIdent Ident
    deriving Show

data FuncSig = FuncSig Ident Type deriving Show

data FuncDef = FuncDef Ident [Ident] Expr deriving Show

data FuncInfo = ISig FuncSig
    | IDef FuncDef
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

funcSigP :: Parser Char FuncSig
funcSigP = do
    name <- identP
    _ <- noSpaceC $ charP ':'
    functype <- funcTypeP
    return $ FuncSig name functype 

funcDefP :: Parser Char FuncDef
funcDefP = do
    name <- identP
    args <- manyC identP
    _ <- noSpaceC $ charP '='
    expr <- exprP
    return $ FuncDef name args expr

funcInfoP :: Parser Char FuncInfo
funcInfoP = (funcDefP >>= return . IDef) <|> (funcSigP >>= return . ISig)

progP :: Parser Char AST
progP = many1C funcInfoP >>= return . Prog

-- TODO (BIG): Type checking
data Type = TBool
    | TInt
    | TFunc Type Type
    deriving (Show, Eq)

newtype TypeEnv = TypeEnv {getType :: Ident -> Maybe Type}

teEmpty :: TypeEnv
teEmpty = TypeEnv $ \_ -> Nothing

addType :: Ident -> Type -> TypeEnv -> TypeEnv
addType ident typename (TypeEnv lookupType) = TypeEnv newLookupType
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

data TypeError = UsedBeforeDeclared Ident
    | TypeMismatch Type Type
    | TooManyArguments Ident
    | NoTypeSig Ident
    deriving Show

type TypeChecker a = TypeEnv -> a -> Either TypeError (Type, TypeEnv)

typeCheckVar :: TypeChecker Var
typeCheckVar env (VIntLit _) = Right (TInt, env)
typeCheckVar env (VIdent ident) = case getType env ident of
    Nothing -> Left $ UsedBeforeDeclared ident
    Just t -> Right (t, env)

typeCheckExpr :: TypeChecker Expr
typeCheckExpr env (EVar var) = typeCheckVar env var
typeCheckExpr env (EPlus var expr) = case (typeCheckVar env var, typeCheckExpr env expr) of
    (Left e, _)                        -> Left e
    (_, Left e)                        -> Left e
    (Right (TInt, _), Right (TInt, _)) -> Right (TInt, env)
    (Right (TInt, _), Right (t, _))    -> Left $ TypeMismatch TInt t 
    (Right (t, _), _)                  -> Left $ TypeMismatch TInt t

sortTypes :: TypeEnv -> Ident -> [Ident] -> Either TypeError (Type, TypeEnv)
sortTypes env name args = case getType env name of
    Nothing -> Left $ NoTypeSig name
    Just typename -> sortTypes' env name typename args
    where
        sortTypes' e n t [] = Right (t, e)
        sortTypes' e n t (a:as) = case t of
            TInt -> Left $ TooManyArguments n
            TBool -> Left $ TooManyArguments n
            TFunc t1 t2 -> sortTypes' (addType a t1 e) n t2 as

typeCheckFuncDef :: TypeChecker FuncDef
typeCheckFuncDef env (FuncDef name args expr) = case sortTypes env name args of
    Left e -> Left e
    Right (expectedExprT, newEnv) -> case typeCheckExpr newEnv expr of
        Left e' -> Left e'
        Right (t, _) -> if expectedExprT == t then
            Right (t, env)
        else
            Left $ TypeMismatch expectedExprT t

typeCheckProg :: AST -> Either TypeError ()
typeCheckProg (Prog infos) = typeCheckProg' newEnv defs
    where
        (newEnv, defs) = consumeSigs teEmpty infos
        typeCheckProg' e [] = Right ()
        typeCheckProg' e (d:rest) = case typeCheckFuncDef e d of
            Left err -> Left err
            Right _ -> typeCheckProg' e rest

consumeSigs :: TypeEnv -> [FuncInfo] -> (TypeEnv, [FuncDef])
consumeSigs env infos = consumeSigs' env infos []
    where
        consumeSigs' e [] defs = (e, defs)
        consumeSigs' e (ISig (FuncSig name typename):is) defs = consumeSigs' (addType name typename e) is defs
        consumeSigs' e (IDef d:is) defs = consumeSigs' e is (d:defs)