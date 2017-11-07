{-# OPTIONS_GHC -Wall #-}

module Print (
  testPrint,
  printType,
  printExpr,
  printProgram
  ) where

import Model

import Data.List (intercalate)

import Test.Hspec

printProgram :: Program -> String
printProgram stmts = intercalate "\n\n" (map printStmt stmts)

printStmt :: Stmt -> String
printStmt (Expr e) = printExpr e
printStmt (DefData tyname variants) =
  "data " ++ tyname ++ " = " ++ (intercalate " | " (map printVariant variants))
printStmt (DefVal name maybeTy cases) = intercalate "\n" . filter (/= "") $ [
  printTypeDecl name maybeTy,
  printCases name cases
  ]

printTypeDecl :: Lowercase -> Maybe Type -> String
printTypeDecl name Nothing = name ++ " :: _"
printTypeDecl name (Just ty) = name ++ " :: " ++ printType ty

printCases :: Lowercase -> [Case] -> String
printCases name [Case [] e] = name ++ " = " ++ printExpr e
printCases name _ = name ++ " = ..."

printVariant :: Variant -> String
printVariant (Variant name args) = name ++ " " ++ unwords (map printTypeArg args)

printType :: Type -> String
printType (T name) = name
printType (F inn out) = printTypeArg inn ++ " -> " ++ printType out

printTypeArg :: Type -> String
printTypeArg (T name) = name
printTypeArg t = "(" ++ printType t ++ ")"

printExpr :: Expr -> String
printExpr (Var name) = name
printExpr (Cst name) = name
printExpr (App callee arg) = printExpr callee ++ " " ++ printArg arg

printArg :: Expr -> String
printArg (Var name) = name
printArg (Cst name) = name
printArg (App callee arg) = "(" ++ printExpr (App callee arg) ++ ")"


testPrint :: Spec
testPrint = do
  it "examples" $ do
    printExpr (App (App (Cst "Cons") (Var "x")) (Var "y")) `shouldBe` "Cons x y"
    printExpr (App (App (Cst "Cons")
                    (App (Var "head") (Var "lst")))
               (App (Var "tail") (Var "lst")))
      `shouldBe` "Cons (head lst) (tail lst)"
    printExpr (App (Cst "Succ")
               (App (Cst "Succ")
                (App (Cst "Succ")
                 (App (Cst "Succ") (Cst "Zero")))))
      `shouldBe` "Succ (Succ (Succ (Succ Zero)))"
  
