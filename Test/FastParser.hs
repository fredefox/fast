{-
 - Testing "Fast"-Parser
 - =====================
 -
 - This module implements tests for the Fast-parser.
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
module Test.FastParser () where

import qualified FastAST as AST
import qualified FastParser as Parser
import Text.PrettyPrint.HughesPJ
import Test.QuickCheck

printName :: AST.Name -> Doc
printName = text

printTerm :: AST.Term -> Doc
-- TODO
-- Please also note that there is no parser generating a `Term`
printTerm (AST.Term name values) = undefined

printValue :: AST.Value -> Doc
-- TODO
printValue (AST.TermValue t) = undefined -- No parse generating this
printValue (AST.IntValue i) = undefined
printValue (AST.StringValue s) = undefined

printExpr :: AST.Expr -> Doc
printExpr (AST.IntConst i) = undefined
printExpr (AST.StringConst s) = undefined
printExpr (AST.TermLiteral n es) = undefined
printExpr (AST.Self) = undefined
printExpr (AST.Plus e0 e1) = undefined
printExpr (AST.Minus e0 e1) = undefined
printExpr (AST.Times e0 e1) = undefined
printExpr (AST.DividedBy e0 e1) = undefined
printExpr (AST.Return e) = undefined
printExpr (AST.SetField fld val) = undefined
printExpr (AST.SetVar var val) = undefined
printExpr (AST.ReadVar var) = undefined
printExpr (AST.ReadField fld) = undefined
printExpr (AST.Match key values) = undefined
printExpr (AST.SendMessage rcpt msg) = undefined
printExpr (AST.CallMethod rcpt meth args) = undefined
printExpr (AST.New n exprs) = undefined

printCase :: AST.Case -> Doc
printCase (ptrn, es) = undefined

printPattern :: AST.Pattern -> Doc
printPattern (AST.ConstInt i) = undefined
printPattern (AST.ConstString s) = undefined
printPattern (AST.TermPattern n ns) = undefined
printPattern (AST.AnyValue val) = undefined

printClassDecl :: AST.ClassDecl -> Doc
printClassDecl (AST.ClassDecl n cons methds recv) = undefined

printReceiveDecl :: AST.ReceiveDecl -> Doc
printReceiveDecl (AST.ReceiveDecl param body) = undefined

printNamedMethodDecl :: AST.NamedMethodDecl -> Doc
printNamedMethodDecl (AST.NamedMethodDecl name decl) = undefined

printMethodDecl :: AST.MethodDecl -> Doc
printMethodDecl (AST.MethodDecl params body) = undefined

printProg :: AST.Prog -> Doc
printProg classDecls = undefined
