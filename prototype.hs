
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

import Test.Hspec



-- data model
data Expr = Var String
          | App Expr Expr
          | Lam String Expr
          deriving (Show, Eq)
            

-- parsing
type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser sourceName input =
    runIndent sourceName (runParserT aParser () sourceName input)

pIden :: IParser String
pIden = do
  c <- letter
  cs <- many alphaNum
  spaces
  return (c:cs)

pLam :: IParser Expr
pLam = do
  char '\\'
  spaces
  name <- pIden
  spaces
  char '.'
  spaces
  body <- pExpr
  return (Lam name body)

pFactor :: IParser Expr
pFactor = pLam
          <|> (Var <$> pIden)
          <|> (do char '('
                  spaces
                  e <- pExpr
                  spaces
                  char ')'
                  spaces
                  return e)

pExpr :: IParser Expr
pExpr = chainl1 pFactor (return App)

test :: IO ()
test = hspec $ do
  describe "pExpr" $ do
    let means s e =
          it (s ++ " `means` " ++ show e) $ do
            iParse pExpr "example" s `shouldBe` (Right e)
    "x" `means` (Var "x")
    "f x" `means` (App (Var "f") (Var "x"))
    "f x y" `means` (App (App (Var "f") (Var "x")) (Var "y"))
    "f x y" `means` (App (App (Var "f") (Var "x")) (Var "y"))
    "f x y \\z.q" `means` (App (App (App (Var "f") (Var "x")) (Var "y")) (Lam "z" (Var "q")))


