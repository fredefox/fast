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
module Test.FastPrinter where

import qualified FastAST as AST
import qualified FastParser as Parser

import Text.PrettyPrint.HughesPJ

import Data.List (intersperse)

printName :: AST.Name -> Doc
printName n = text n

printInteger :: Integer -> Doc
printInteger i = text $ show i

printQuotedString :: String -> Doc
-- Following must be false:
--
--     '"' `elem` s
--
printQuotedString s = text $ '"' : s' ++ ['"'] where
-- To accomplish this, I could just filter out the quotes. But for now I'll
-- actually let my printer be able to produce things that are not valid
-- according to the BNF. It must then be the QuickChecks responsibility of not
-- producing this sort of input. An alternate thing I could do would be to
-- throw an error if illegal input is given to various functions from this
-- library. But since it is not library code I will ignore this.
    s' = s -- filter (\c -> c /= '"') s

printTerm :: AST.Term -> Doc
-- TODO
-- Please also note that there is no parser generating a `Term`
printTerm (AST.Term name values) = error "Error printing undefined for `Term _ _`"

printValue :: AST.Value -> Doc
-- TODO
printValue (AST.TermValue t)
    = error "Error printing undefined for `TermValue _`" -- No parse generating this
printValue (AST.IntValue i) = error "Error printing undefined for `IntValue _`"
printValue (AST.StringValue s) = error "Error printing undefined for `StringValue`"

printExpr :: AST.Expr -> Doc
printExpr (AST.IntConst i) = text $ show i
printExpr (AST.StringConst s) = printQuotedString s
printExpr (AST.TermLiteral n es) = printName n
    <> text "("
    <> (cat $ Data.List.intersperse comma $ fmap printExpr es)
    <> text ")"
printExpr (AST.Self) = text "self"
printExpr (AST.Plus e0 e1) = printExpr e0 <> text "+" <> printExpr e1
printExpr (AST.Minus e0 e1) = printExpr e0 <> text "-" <> printExpr e1
printExpr (AST.Times e0 e1) = printExpr e0 <> text "*" <> printExpr e1
printExpr (AST.DividedBy e0 e1) = printExpr e0 <> text "/" <> printExpr e1
-- TODO: Nothing parses this
printExpr (AST.Return e) = text "return " <> printExpr e
printExpr (AST.SetField fld val) = text "set "
    <> text "self"
    <> text "."
    <> printName fld
    <> text "="
    <> printExpr val
printExpr (AST.SetVar var val) = text "set "
    <> text "self"
    <> text "."
    <> printName var
    <> text "="
    <> printExpr val
-- TODO: Nothing parses this
printExpr (AST.ReadVar var) = error "Error printing undefined for `ReadVar _`"
printExpr (AST.ReadField fld) = text "self"
    <> text "."
    <> printName fld
printExpr (AST.Match key values) = text "match "
    <> printExpr key
    <> text "{"
    <> (cat $ fmap printCase values)
    <> text "}"
printExpr (AST.SendMessage rcpt msg) = text "send "
    <> text "("
    <> printExpr rcpt
    <> text ","
    <> printExpr msg
    <> text ")"
printExpr (AST.CallMethod rcpt meth args) = printExpr rcpt
    <> text "."
    <> printName meth
    <> (cat $ Data.List.intersperse semi $ fmap printExpr args)
    <> text ")"
printExpr (AST.New n args) = text "new "
    <> printName n
    <> text "("
    <> (cat $ Data.List.intersperse comma $ fmap printExpr args)
    <> text ")"

printCase :: AST.Case -> Doc
printCase (ptrn, es) = printPattern ptrn
    <> text "->"
    <> text "{"
    <> (cat $ Data.List.intersperse semi $ fmap printExpr es)
    <> text "}"

printPattern :: AST.Pattern -> Doc
printPattern (AST.ConstInt i) = printInteger i
printPattern (AST.ConstString s) = printQuotedString s
printPattern (AST.TermPattern n ns) = printName n
    <> text "("
    <> (cat $ Data.List.intersperse comma $ fmap printName ns)
    <> text ")"
printPattern (AST.AnyValue val) = printName val

printClassDecl :: AST.ClassDecl -> Doc
printClassDecl (AST.ClassDecl clsNm cons methds recv) = text "class "
    <> printName clsNm
    <> text "{"
    <> printCons cons
    <> (cat $ fmap printNamedMethodDecl methds)
    <> printRecv recv
    <> text "}" where
        printCons Nothing = text ""
        printCons (Just c) = printMethodDecl c
        printRecv Nothing = text ""
        printRecv (Just r) = printReceiveDecl r

printReceiveDecl :: AST.ReceiveDecl -> Doc
printReceiveDecl (AST.ReceiveDecl param body) = text "receive "
    <> text "("
    <> printName param
    <> text ")"
    <> text "{"
    <> (cat $ Data.List.intersperse semi $ fmap printExpr body)
    <> text "}"


printNamedMethodDecl :: AST.NamedMethodDecl -> Doc
printNamedMethodDecl (AST.NamedMethodDecl name decl) = printName name
    <> text "("
    <> (cat $ Data.List.intersperse comma $ fmap printName params)
    <> text ")"
    <> text "{"
    <> (cat $ Data.List.intersperse semi $ fmap printExpr body)
    <> text "}" where
        AST.MethodDecl params body = decl

printMethodDecl :: AST.MethodDecl -> Doc
printMethodDecl (AST.MethodDecl params body) = text "new "
    <> text "("
    <> (cat $ Data.List.intersperse comma $ fmap printName params)
    <> text ")"
    <> text "{"
    <> (cat $ Data.List.intersperse semi $ fmap printExpr body)
    <> text "}"


printProg :: AST.Prog -> Doc
printProg classDecls
    = (cat $ fmap printClassDecl classDecls)
