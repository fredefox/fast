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
import Control.Applicative hiding ((<|>))
import Control.Monad
import Text.Parsec
import Test.QuickCheck
import Text.PrettyPrint.HughesPJ (render)

{-
 - This test-module should only test methods that the API-exposes. But for
 - debugging purposes I have tried to also test the sub-expressions. To make
 - this class compile we must declare those methods since we can't actually
 - access them via the import-statement.
 -}
name = undefined
quotedString = undefined

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

instance Arbitrary Pattern where
    arbitrary = oneof
        [ liftM ConstInt arbitrary
        , liftM ConstString arbitrary
        , liftM2 TermPattern arbitrary arbitrary
        , liftM AnyValue arbitrary
        ]

instance Arbitrary Expr where
    arbitrary = oneof
        [ liftM IntConst arbitrary
        , liftM StringConst arbitrary
        , liftM2 TermLiteral arbitrary arbitrary
        , pure Self
        , liftM2 Plus arbitrary arbitrary
        , liftM2 Minus arbitrary arbitrary
        , liftM2 Times arbitrary arbitrary
        , liftM2 DividedBy arbitrary arbitrary
        , liftM Return arbitrary
        , liftM2 SetField arbitrary arbitrary
        , liftM2 SetVar arbitrary arbitrary
        , liftM ReadVar arbitrary
        , liftM ReadField arbitrary
        , liftM2 Match arbitrary arbitrary
        , liftM2 SendMessage arbitrary arbitrary
        , liftM3 CallMethod arbitrary arbitrary arbitrary
        , liftM2 New arbitrary arbitrary
        ]

instance Arbitrary MethodDecl where
    arbitrary = liftM2 MethodDecl arbitrary arbitrary

checkProgramParser :: Prog -> Property
checkProgramParser prog =
    case (parseString inp) of
        Left _ -> property False
        Right res -> prog === res
    where
        inp = render $ printProg prog
