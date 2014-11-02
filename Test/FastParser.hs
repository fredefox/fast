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
{-
alphaFreqList =
    [ (26, choose ('a', 'z'))
    , (26, choose ('A', 'Z'))
    ]
allCharsFreqList = alphaFreqList ++
    [ (10, choose ('0', '9'))
    , (1, pure '_')
    ]

letter' = frequency alphaFreqList
letterOrDigit = frequency $ alphaFreqList ++ digitFreqList

identifierGenerator = liftM2 (:) letter' $
  sized (\n -> replicateM n letterOrDigit)
-}
nameGenerator = liftM2 (:) gen0 $ sized $ sizedExpr where
    sizedExpr = \n -> replicateM n gen1
    -- Produces legal beginning characters
    gen0 = frequency firstChars
    -- Produces the other legal character a name can contain
    gen1 = frequency legalChars
    -- And here are the frequency lists for these characters
    firstChars =
        [ (26, choose ('a', 'z'))
        , (26, choose ('A', 'Z'))
        ]
    legalChars = firstChars ++
        [ (10, choose ('0', '9'))
        , (1, pure '_')
        ]

quotedStringGenerator = liftM2 (:) quot rest where
    rest = liftM2 (++) nonQuots ( fmap (\i -> [i]) quot )
    quot :: Gen Char
    quot = pure '"'
    nonQuots :: Gen String
    nonQuots = listOf $ elements $ ['\32'..'\33'] ++ ['\35'..'\126']

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
        , liftM ConstString quotedStringGenerator
        , liftM2 TermPattern nameGenerator $ listOf nameGenerator
        , liftM AnyValue nameGenerator
        ]

instance Arbitrary Expr where
    arbitrary = oneof
        [ liftM IntConst arbitrary
        , liftM StringConst quotedStringGenerator
        , liftM2 TermLiteral nameGenerator arbitrary
        , pure Self
        , liftM2 Plus arbitrary arbitrary
        , liftM2 Minus arbitrary arbitrary
        , liftM2 Times arbitrary arbitrary
        , liftM2 DividedBy arbitrary arbitrary
        , liftM Return arbitrary
        , liftM2 SetField nameGenerator arbitrary
        , liftM2 SetVar nameGenerator arbitrary
        , liftM ReadVar nameGenerator
        , liftM ReadField nameGenerator
        , liftM2 Match arbitrary arbitrary
        , liftM2 SendMessage arbitrary arbitrary
        , liftM3 CallMethod arbitrary nameGenerator arbitrary
        , liftM2 New nameGenerator arbitrary
        ]

f :: Gen FastName -> Gen FastName
f = id

instance Arbitrary ReceiveDecl where
    arbitrary = liftM2 ReceiveDecl nameGenerator arbitrary

instance Arbitrary NamedMethodDecl where
    arbitrary = liftM2 NamedMethodDecl nameGenerator arbitrary

instance Arbitrary ClassDecl where
    arbitrary = liftM4 ClassDecl nameGenerator arbitrary (listOf arbitrary) arbitrary

instance Arbitrary MethodDecl where
    arbitrary = liftM2 MethodDecl (listOf nameGenerator) arbitrary

checkProgramParser :: Prog -> Property
checkProgramParser prog =
    case (parseString inp) of
        Left _ -> property False
        Right res -> prog === res
    where
        inp = render $ printProg prog
