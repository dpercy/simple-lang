
import Text.Parsec hiding (State, token)
import Text.Parsec.Indent
import Control.Monad.State

import Data.List

import Test.Hspec



-- data model
type Program = [Def]

-- TODO type parameters
data Def = DefVal Lowercase [Case]
         | DefData Uppercase [Variant]
         -- TODO introduce type annotations; group them with definitions
         deriving (Show, Eq)

data Case = Case [Pattern] Expr
          deriving (Show, Eq)

data Variant = Variant Uppercase [Type]
             deriving (Show, Eq)

data Type = T Uppercase
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


reservedWords :: [String]
reservedWords = [ "data", "type", "case", "match" ]

pVar :: IParser Lowercase
pVar = try $ do v <- tokLower
                guard (not (v `elem` reservedWords))
                return v

pWord :: String -> IParser ()
pWord word = try $ do v <- tokLower
                      void $ guard (v == word)

kwData = pWord "data"


pParens :: IParser a -> IParser a
pParens parser = (do token (char '(')
                     e <- parser
                     token (char ')')
                     return e)


pPattern :: IParser Pattern
pPattern = (Hole <$> pVar)
           <|> pParens (Constructor <$> tokUpper <*> many pPattern)
           <|> Constructor <$> tokUpper <*> pure []


pExpr :: IParser Expr
pExpr = chainl1 pFactor (return App)
  where pFactor :: IParser Expr
        pFactor = pParens pExpr
                  <|> (Var <$> pVar)
                  <|> (Cst <$> tokUpper)


pDefVal :: IParser Def
pDefVal = do
  (name, case0) <- withPos $ do name <- pVar
                                case0 <- pCase
                                return (name, case0)
  cases <- many $ withPos $ do pWord name
                               pCase
  return $ DefVal name (case0:cases)

pCase :: IParser Case
pCase = do
  pats <- many pPattern
  token (char '=')
  expr <- pExpr
  return $ Case pats expr


pDefData :: IParser Def
pDefData = withPos $ do
  kwData
  typeName <- tokUpper
  token (char '=')
  variants <- pVariant `sepBy` token (char '|')
  return $ DefData typeName variants
    where pVariant :: IParser Variant
          pVariant = Variant <$> tokUpper <*> many (T <$> tokUpper)


pDef :: IParser Def
pDef = pDefData <|> pDefVal


pProgram :: IParser Program
pProgram = many pDef


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
  describe "pProgram" $ do
    let means s prog =
          it (prettyLines s) $ do
            iParse pProgram "example" s `shouldBe` (Right prog)
    "x = y" `means` [DefVal "x" [Case [] (Var "y")]]
    "x = y\nx = z" `means` [DefVal "x" [Case [] (Var "y"), Case [] (Var "z")]]
    "x = y\na = z" `means` [DefVal "x" [Case [] (Var "y")], DefVal "a" [Case [] (Var "z")]]
    -- Note this program is invalid because x is defined twice.
    "x = y\na = z\nx = q" `means` [ DefVal "x" [ Case [] (Var "y")]
                                  , DefVal "a" [ Case [] (Var "z")]
                                  , DefVal "x" [ Case [] (Var "q")] ]

    "f x = x" `means` [ DefVal "f" [ Case [Hole "x"] (Var "x") ]]
    "f (S x) = x" `means` [ DefVal "f" [ Case [Constructor "S" [Hole "x"]] (Var "x") ]]
    "f (Cons hd tl) = x" `means` [ DefVal "f" [ Case [Constructor "Cons" [ Hole "hd"
                                                                         , Hole "tl"]]
                                                (Var "x") ]]
    "data Nat = O | S Nat" `means` [ DefData "Nat" [ Variant "O" []
                                                   , Variant "S" [T "Nat"]]]
    "data NatList = Nil | Cons Nat NatList" `means` [ DefData "NatList" [ Variant "Nil" []
                                                                        , Variant "Cons" [T "Nat", T "NatList"]]]

