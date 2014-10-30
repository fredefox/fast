{-
 - Parser for Fast
 - ===============
 -
 - This module implements a parser for the language Fast.
 -
 - This module follows the specfication outlined in the [hand-out].
 -
 - Written by Frederik Hanghøj Iversen
 - for the course Advanced Programming
 - at The University of Copenhagen 2014
 -
 -     me@fredefox.eu /^._
 -      ,___,--~~~~--' /'~
 -      `~--~\ )___,)/'
 -          (/\\_  (/\\_
 -
 -}
module FastParser (Error, parseString, parseFile) where

import FastAST
import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)

integer :: Parser Integer
integer = fmap read $ many1 digit

quotedString :: Parser String
quotedString = do
    char '"'
    manyTill anyChar $ char '"'

name :: Parser Name
name = do
    c <- letter
    s <- many alphaNum <|> string "_"
    return $ c:s

expr :: Parser Expr
expr = expr0 <|> expr1 where
    expr0 = (integer >>= \i -> return $ IntConst i)
        <|> (name >>= \n -> return $ StringConst n)
        <|> fnApplication
        <|> (string "self" >>= (const $ return Self))
        <|> (string "return" >> expr)
        <|> setField
        <|> setVar where
            fnApplication = do
                n <- name
                char '('
                exprs <- expr `sepBy` char ','
                return $ TermLiteral n exprs
            setField = do
                string "set"
                string "self" >> char '.'
                n <- name
                char '='
                e <- expr
                return $ SetField n e
            setVar = do
                string "set" >> string "self" >> char '.'
                n <- name
                char '='
                e <- expr
                return $ SetVar n e
    expr1 = undefined

consDecl :: Parser ConstructorDecl
consDecl = do
    string "new"
    char '('
    p <- name `sepBy` char ','
    char '{'
    e <- expr `sepBy` char ';'
    char '}'
    return MethodDecl {
        methodParameters = p,
        methodBody = e
    }

namedMethodDecl :: Parser NamedMethodDecl
namedMethodDecl = do
    n <- name
    char '('
    params <- name `sepBy` char ','
    char ')'
    char '{'
    exprs <- expr `sepBy` char ';'
    char '}'
    return $ NamedMethodDecl n MethodDecl {
        methodParameters = params,
        methodBody = exprs
    }

receiveDecl :: Parser ReceiveDecl
receiveDecl = do
    string "receive"
    char '('
    p <- name
    char ')'
    char '{'
    exprs <- expr `sepBy` char ';'
    char '}'
    return $ ReceiveDecl {
        receiveParam = p,
        receiveBody = exprs
    }

classDecl :: Parser ClassDecl
classDecl = do
    string "class"
    n <- name
    char '{'
    cons <- optionMaybe consDecl
    methods <- many namedMethodDecl
    recv <- optionMaybe receiveDecl
    return ClassDecl {
        className = n,
        classConstructor = cons,
        classMethods = methods,
        classReceive = recv
    }

prog :: Parser Prog
prog = many classDecl

{-
 - API
 - ===
 -
 - Below the API is defined.
 -
 -}
type Error = ParseError

parseString :: String -> Either Error Prog
parseString = parse prog "Fast"

parseFile :: FilePath -> IO (Either Error Prog)
parseFile path = fmap parseString $ readFile path
