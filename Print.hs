module Print (
  testPrint,
  printExpr
  ) where

import Model

import Test.Hspec

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
  
