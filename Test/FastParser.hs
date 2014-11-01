{-
 - Testing "Fast"-Parser
 - =====================
 -
 - This module implements pretty printing Fast-expressions.
 -
 - NB: This is not really a part of the library and should maybe be placed in
 - some auxillary place.
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
module Test.FastParser where

import FastParser
import FastAST
import FastPrinter
import Control.Applicative
import Text.Parsec
import Test.QuickCheck
import Text.PrettyPrint.HughesPJ (render)

newtype FastName = FS Name
    deriving (Eq, Show)

instance Arbitrary FastName where
    arbitrary = fmap FS $ listOf $ elements alphaNum where
        alpha = ['a'..'z']
            ++ ['A'..'Z']
        alphaNum = alpha ++ ['0'..'9']

checkNameParser :: FastName -> Property
checkNameParser (FS n) =
    case n of
        c:cs ->
            c `elem` ['a'..'z'] ++ ['A'..'Z'] ==>
                case (parse name "" inp) of
                    Left _ -> property False
                    Right res -> inp === res
                where
                    inp = render $ printName n
        otherwise -> property True

checkQuotedStringParser :: QuotedString -> Property
checkQuotedStringParser s =
    case (parse quotedString "" inp) of
        Left _ -> property False
        Right res -> inp === '"' : res ++ ['"']
    where
        inp = s'
        QS s' = s

nonQuotes = ['\32', '\33'] ++ ['\35'..'\126']

newtype QuotedString = QS String
    deriving (Eq, Show)

instance Arbitrary QuotedString where
    arbitrary = fmap enquote $ listOf $ elements nonQuotes where
        enquote = (\s -> QS $ '"' : s ++ ['"'])

checkProgramParser :: Prog -> Property
checkProgramParser prog =
    case (parseString inp) of
        Left _ -> property False
        Right res -> prog === res
    where
        inp = render $ printProg prog
