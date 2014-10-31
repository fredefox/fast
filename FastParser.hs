{-
 - Parser for "Fast"
 - =================
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

fastCase :: Parser Case
fastCase = undefined

{-
 - This method has a rather intricate structure. It is preserved in this single
 - block to illustrate how the different components interact. Please refer to
 - the [factorized bnf](./fast-factorized.bnf) and [the parser-notes]. It is
 - structured like this to account for the fact that the original definition
 - ([./fast.bnf]) was lefft-recursive - meaning that it had a possible parse
 - that would never terminate since it could always choose the production rule
 - where it refered to itself as the first element in the production-sequence.
 -
 - In [the parser-notes] factorization of left-recursive nonterminals are only
 - shown for one class of production-rules. Namely those on the form:
 -
 -     A ::= A g_1 | ... | A g_m | f_1 | ... | f_n
 -
 - Since this production-rule is *not* on that form it had to be chopped up
 - into smaller pieces.
 -}
expr :: Parser Expr
expr = expr0 <|> expr1 where
    expr0 = (integer >>= \i -> return $ IntConst i)
        <|> (quotedString >>= \s -> return $ StringConst s)
        <|> (name >>= \n -> return $ TermLiteral n [])
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
    expr1 = expr1a <|> expr1b where
        expr1a = match <|> send <|> self where
            match = do
                string "match"
                e <- expr
                char '{'
                cs <- many fastCase
                char '}'
                expr1Opt $ Match e cs
            send = do
                string "send"
                char '('
                e0 <- expr
                char ','
                e1 <- expr
                char ')'
                expr1Opt $ SendMessage e0 e1
            self = do
                string "self"
                char '.'
                n <- name
                return $ ReadField n
        expr1b = newDef <|> par where
            newDef = do
                string "new"
                n <- name
                char '('
                args <- expr `sepBy` char ','
                char ')'
                e <- expr1Opt $ New n args
                expr1bOpt e
            par = do
                char '('
                e <- expr
                char ')'
                e' <- expr1Opt e
                expr1bOpt e
            -- Method application (CallMethod)
            expr1bOpt :: Expr -> Parser Expr
            expr1bOpt inherited = invocation
                <|> return inherited where
                    invocation = do
                        char '.'
                        n <- name
                        char '('
                        args <- expr `sepBy` char ','
                        char ')'
                        expr1Opt $ CallMethod inherited n args
        -- Arithmetic
        expr1Opt :: Expr -> Parser Expr
        expr1Opt inherited = plus
            <|> minus
            <|> mult
            <|> division
            <|> return inherited where
                plus = do
                    char '+'
                    e <- expr
                    expr1Opt e
                minus = do
                    char '-'
                    e <- expr
                    expr1Opt e
                mult = do
                    char '*'
                    e <- expr
                    expr1Opt e
                division = do
                    char '/'
                    e <- expr
                    expr1Opt e

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
