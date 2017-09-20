
import Text.Parsec hiding (State, token)
import Text.Parsec.Indent
import Control.Monad.State

import Data.List

import Test.Hspec



-- data model
type Program = [Def]

data Def = DefVal [Equation]
         -- | DefData ...
         deriving (Show, Eq)

data Equation = Equation { name :: Lowercase
                         , params :: [Pattern]
                         , value :: Expr }
              deriving (Show, Eq)

data Pattern = Hole Lowercase
             | Constructor Uppercase [Pattern]
             deriving (Show, Eq)

data Expr = Var Lowercase
          | Cst Uppercase
          | App Expr Expr
          deriving (Show, Eq)

type Lowercase = String
type Uppercase = String



-- parsing
type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser sourceName input =
    runIndent sourceName (runParserT (spaces >> aParser) () sourceName input)

{-
Make sure to follow the "lexeme" convention:
- every token (terminal in the grammar) starts with 'tok'
- every token checks indentation
- every token consumes spaces after it

This ensures spaces are handled efficiently,
and indentation is checked in all cases.

`token` is a helper function that turns any parsec parser
into a "tok*" parser that follows this convention.
-}
token :: IParser a -> IParser a
token p = do
  sameOrIndented
  v <- p
  spaces
  return v

tokLower :: IParser Lowercase
tokLower = token ((:) <$> lower <*> many alphaNum)

tokUpper :: IParser Uppercase
tokUpper = token ((:) <$> upper <*> many alphaNum)




pParens :: IParser a -> IParser a
pParens parser = (do token (char '(')
                     e <- parser
                     token (char ')')
                     return e)


pPattern :: IParser Pattern
pPattern = (Hole <$> tokLower)
           <|> pParens (Constructor <$> tokUpper <*> many pPattern)
           <|> Constructor <$> tokUpper <*> pure []


pExpr :: IParser Expr
pExpr = chainl1 pFactor (return App)
  where pFactor :: IParser Expr
        pFactor = pParens pExpr
                  <|> (Var <$> tokLower)
                  <|> (Cst <$> tokUpper)


pEquation :: IParser Equation
pEquation = withPos $ do
  name <- tokLower
  params <- same >> many pPattern
  void $ same >> char '='
  spaces
  e <- pExpr
  return $ Equation name params e


pEquations :: IParser [Equation]
pEquations = many $ do
  eq <- pEquation
  spaces
  return eq


test :: IO ()
test = hspec $ do
  let prettyLines s = (intercalate "\t" (map show $ lines s))
  describe "pExpr" $ do
    let means s e =
          it s $ do
            iParse pExpr "example" s `shouldBe` (Right e)
    "x" `means` (Var "x")
    "f x" `means` (App (Var "f") (Var "x"))
    "f x y" `means` (App (App (Var "f") (Var "x")) (Var "y"))
    "f x y" `means` (App (App (Var "f") (Var "x")) (Var "y"))
    "f (x y)" `means` (App (Var "f") (App (Var "x") (Var "y")))
  describe "pEquations" $ do
    let means s eqs =
          it (prettyLines s) $ do
            iParse pEquations "example" s `shouldBe` (Right eqs)
    "x = y" `means` [Equation "x" [] (Var "y")]
    "x =\n y" `means` [Equation "x" [] (Var "y")]
    "f = a\ng = b" `means` [Equation "f" [] (Var "a"), Equation "g" [] (Var "b")]
    " f = a\ng = b" `means` [Equation "f" [] (Var "a"), Equation "g" [] (Var "b")]
    "f = a\ng =\n b" `means` [Equation "f" [] (Var "a"), Equation "g" [] (Var "b")]
    "f x = z" `means` [Equation "f" [(Hole "x")] (Var "z")]
    "f (F x) = z" `means` [Equation "f" [Constructor "F" [Hole "x"]] (Var "z")]
    "f (F x) Q = z" `means` [Equation "f" [Constructor "F" [Hole "x"], Constructor "Q" []] (Var "z")]
    -- TODO more
    {-
  describe "pProgram" $ do
    let means s prog =
          it (prettyLines s) $ do
            iParse pEquations "example" s `shouldBe` (Right prog)
    "x = y" `means` [DefVal [Equation "x" [] (Var "y")]]

-}
