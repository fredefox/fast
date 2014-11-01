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
import Control.Applicative ((<*))
import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)

{-
 - Helper methods
 -}
parseAll :: Parser a -> String -> Either Error a
parseAll prs = parse (prs <* (spaces >> eof)) "Fast"

{-
 - Custom Parser-combinators
 -}
symbol :: Parser p -> Parser p
symbol p = spaces >> p

{-
 - These are the more "atomic" parsers. It is the parsers for the values
 - mentioned outside the BNF-proper.
 -}
integer :: Parser Integer
integer = fmap read $ integer' where
    integer' :: Parser String
    integer' = do
        s <- optionMaybe $ char '-'
        ds <- many1 digit
        case s of
            Nothing -> return ds
            Just a -> return $ a:ds

quotedString :: Parser String
quotedString = do
    char '"'
    manyTill anyChar $ char '"'

{-
 - The parsers that follow are all parsers that parse some produciton-rule in
 - the BNF.
 -}
keywords :: [String]
keywords = ["self", "class", "new", "receive", "send", "match", "set"]

name :: Parser Name
name = try $ do
    c <- letter
    cs <- many alphaNum <|> string "_"
    let s = c:cs in
        if s `elem` keywords then
            unexpected s
        else
            return s

pattern :: Parser Pattern
pattern = (integer >>= \i -> return $ ConstInt i)
    <|> (quotedString >>= \s -> return $ ConstString s)
    <|> termPattern
    <|> (name >>= \n -> return $ AnyValue n) where
        termPattern = do
            n <- name
            char '('
            ns <- name `sepBy` char ','
            char ')'
            return $ TermPattern n ns

fastCase :: Parser Case
fastCase = do
    p <- pattern
    string "->"
    char '{'
    es <- expr `sepBy` char ';'
    char '}'
    return (p, es)

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
        -- This parser will *succeed* so `try`ing it won't help it already
        -- succeeded and consumed input. The problem with this is that just
        -- below there is a rule that starts with paranthesis. If the
        -- name-parser already succeeded and we see a parenthesis, we have no
        -- way of going back and not using that parse. We should therefore
        -- parse a name in *any* case first, and then see if there's a
        -- parenthesis.
        --
        -- The result of this is that there are things that can't be parsed
        -- with my current design. I need to structure my code in a very
        -- complex way such that e.g. `name` is applied (if possible) and when
        -- applied it will then try parsing on and if it fails, well then it
        -- must check to see if there is a parenthesis.
        <|> (name >>= \n -> return $ TermLiteral n [])
        <|> termLiteral
        <|> (string "self" >>= (const $ return Self))
        <|> (string "return" >> expr)
        <|> setField
        <|> setVar where
            termLiteral = do
                n <- name
                char '('
                exprs <- expr `sepBy` char ','
                char ')'
                return $ TermLiteral n exprs
            setField = do
                string "set"
                space
                string "self" >> char '.'
                fld <- name
                char '='
                val <- expr
                return $ SetField fld val
            setVar = do
                string "set"
                var <- name
                char '='
                val <- expr
                return $ SetVar var val
    expr1 = expr1a <|> expr1b where
        expr1a = match <|> send <|> self where
            match = do
                string "match"
                key <- expr
                char '{'
                values <- many fastCase
                char '}'
                expr1Opt $ Match key values
            send = do
                string "send"
                char '('
                rcpt <- expr
                char ','
                msg <- expr
                char ')'
                expr1Opt $ SendMessage rcpt msg
            self = do
                string "self"
                char '.'
                n <- name
                return $ ReadField n
        expr1b = newDef <|> par where
            newDef = do
                string "new"
                space
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
                        rcpt <- name
                        char '('
                        args <- expr `sepBy` char ','
                        char ')'
                        expr1Opt $ CallMethod inherited rcpt args
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
    space
    char '('
    p <- name `sepBy` char ','
    char ')'
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
    body <- expr `sepBy` char ';'
    char '}'
    return $ NamedMethodDecl n MethodDecl {
        methodParameters = params,
        methodBody = body
    }

receiveDecl :: Parser ReceiveDecl
receiveDecl = do
    string "receive"
    space
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
    space
    clsNm <- name
    char '{'
    -- This parse suffers from the same problem as below, the extra quirk is that
    cons <- optionMaybe consDecl
    -- The class-name might fail with a look-ahead of more than 1. I.e. it has
    -- to look at more than the next characters to decide if it fails. This is
    -- especially the case if it encounters a reserved keyword (like `receive`)
    -- in this case it should not parse anything but just give up.
    methods <- many namedMethodDecl
    recv <- optionMaybe receiveDecl
    char '}'
    return ClassDecl {
        className = clsNm,
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

{-
 - Since our sub-parsers strip leading white-space we must conclude the parse
 - with one application of `eof`.
 -}
parseString :: String -> Either Error Prog
parseString = parseAll prog

parseFile :: FilePath -> IO (Either Error Prog)
parseFile path = fmap parseString $ readFile path
