
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State


-- data model
data Expr = Var String
          | App Expr Expr
          | Lam String Expr
          deriving (Show)
            

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

inputText = "f x"

main :: IO ()
main = do
  case iParse pExpr "example" inputText of
   Left err -> print err
   Right result -> putStrLn ("I parsed: " ++ show result)
