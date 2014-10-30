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

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
type Error = ParseError

prog :: Parser Prog
prog = undefined

parseString :: String -> Either Error Prog
parseString = parse prog "Fast"

parseFile :: FilePath -> IO (Either Error Prog)
parseFile path = fmap parseString $ readFile path
