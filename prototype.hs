
import Text.Parsec hiding (State, token)
import Text.Parsec.Indent
import Control.Monad.State

import Data.List

import Test.Hspec



-- data model
type Program = [Def]

data Def = DefVal Lowercase [Case]
         -- TODO introduce data definitions
         -- | DefData ...
         -- TODO introduce type annotations; group them with definitions
         deriving (Show, Eq)

data Case = Case [Pattern] Expr
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

data Fragment = Equation Lowercase [Pattern] Expr
              deriving (Show, Eq)


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


pEquation :: IParser Fragment
pEquation = withPos $ do
  name <- tokLower
  params <- same >> many pPattern
  void $ same >> char '='
  spaces
  e <- pExpr
  return $ Equation name params e


pFragments :: IParser [Fragment]
pFragments = many pEquation


pProgram :: IParser Program
pProgram = gather <$> pFragments
  where gather frags = map gatherDefVal names
          where name (Equation n _ _) = n
                names = unique $ map name frags
                gatherDefVal n = DefVal n [ Case pats expr
                                          | (Equation n2 pats expr) <- frags, n == n2 ]

unique :: (Ord a, Eq a) => [a] -> [a]
unique = loop []
  where loop seen [] = reverse seen
        loop seen (x:xs) | x `elem` seen = loop seen xs
        loop seen (x:xs) = loop (x:seen) xs


test :: IO ()
test = hspec $ do
  describe "unique" $ do
    it "uniques locally" $ do
      unique [1, 1, 2] `shouldBe` [1, 2]
    it "preserves order" $ do
      unique [2, 1] `shouldBe` [2, 1]
    it "uniques globally" $ do
      unique [1, 1, 2, 1, 5, 5, 4, 5, 4, 4, 3] `shouldBe` [1,2,5,4,3]
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
  describe "pFragments" $ do
    let means s frags =
          it (prettyLines s) $ do
            iParse pFragments "example" s `shouldBe` (Right frags)
    "x = y" `means` [Equation "x" [] (Var "y")]
    "x =\n y" `means` [Equation "x" [] (Var "y")]
    "f = a\ng = b" `means` [Equation "f" [] (Var "a"), Equation "g" [] (Var "b")]
    " f = a\ng = b" `means` [Equation "f" [] (Var "a"), Equation "g" [] (Var "b")]
    "f = a\ng =\n b" `means` [Equation "f" [] (Var "a"), Equation "g" [] (Var "b")]
    "f x = z" `means` [Equation "f" [(Hole "x")] (Var "z")]
    "f (F x) = z" `means` [Equation "f" [Constructor "F" [Hole "x"]] (Var "z")]
    "f (F x) Q = z" `means` [Equation "f" [Constructor "F" [Hole "x"], Constructor "Q" []] (Var "z")]
  describe "pProgram" $ do
    -- pProgram gathers separate equations and groups them by name
    let means s prog =
          it (prettyLines s) $ do
            iParse pProgram "example" s `shouldBe` (Right prog)
    "x = y" `means` [DefVal "x" [Case [] (Var "y")]]
    "x = y\nx = z" `means` [DefVal "x" [Case [] (Var "y"), Case [] (Var "z")]]
    "x = y\na = z" `means` [DefVal "x" [Case [] (Var "y")], DefVal "a" [Case [] (Var "z")]]
    "x = y\na = z\nx = q" `means` [ DefVal "x" [ Case [] (Var "y")
                                               , Case [] (Var "q")]
                                  , DefVal "a" [ Case [] (Var "z")]]

