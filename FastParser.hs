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
import Text.ParserCombinators.Parsec

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

prog :: Parser Prog
prog = undefined

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
