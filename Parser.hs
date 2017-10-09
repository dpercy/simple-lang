{-# OPTIONS_GHC -Wall #-}

module Parser (
  iParse,
  pProgram,
  pExpr,
  testParse,
  ) where


import Control.Monad.State
import Data.List
import Test.Hspec
import Text.Parsec hiding (State, token)
import Text.Parsec.Indent

import Model


-- parsing
type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser name input =
    runIndent name (runParserT (spaces >> aParser) () name input)

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

kwData :: IParser ()
kwData = pWord "data"


pParens :: IParser a -> IParser a
pParens parser = (do void $ token (char '(')
                     e <- parser
                     void $ token (char ')')
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
  name <- lookAhead pVar
  typ <- try (Just <$> pTypeDecl name) <|> return Nothing
  cases <- many1 (pCase name)
  return $ DefVal name typ cases

pTypeDecl :: String -> IParser Type
pTypeDecl name = withPos $ do
  pWord name
  void $ token (string "::")
  pType

pType :: IParser Type
pType = chainl1 pFactor (token (string "->") >> return F)
  where pFactor :: IParser Type
        pFactor = pParens pType
                  <|> (T <$> tokUpper)
        

pCase :: String -> IParser Case
pCase name = withPos $ do
  pWord name
  pats <- many pPattern
  void $ token (char '=')
  expr <- pExpr
  return $ Case pats expr


pDefData :: IParser Def
pDefData = withPos $ do
  kwData
  typeName <- tokUpper
  void $ token (char '=')
  variants <- pVariant `sepBy` token (char '|')
  return $ DefData typeName variants
    where pVariant :: IParser Variant
          pVariant = Variant <$> tokUpper <*> many pType


pDef :: IParser Def
pDef = pDefData <|> pDefVal


pProgram :: IParser Program
pProgram = many pDef


testParse :: Spec
testParse = do
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
    "x = y" `means` [DefVal "x" Nothing [Case [] (Var "y")]]
    "x = y\nx = z" `means` [DefVal "x" Nothing [Case [] (Var "y"), Case [] (Var "z")]]
    "x = y\na = z" `means` [DefVal "x" Nothing [Case [] (Var "y")], DefVal "a" Nothing [Case [] (Var "z")]]
    -- Note this program is invalid because x is defined twice.
    "x = y\na = z\nx = q" `means` [ DefVal "x" Nothing [ Case [] (Var "y")]
                                  , DefVal "a" Nothing [ Case [] (Var "z")]
                                  , DefVal "x" Nothing [ Case [] (Var "q")] ]

    "f x = x" `means` [ DefVal "f" Nothing [ Case [Hole "x"] (Var "x") ]]
    "f (S x) = x" `means` [ DefVal "f" Nothing [ Case [Constructor "S" [Hole "x"]] (Var "x") ]]
    "f (Cons hd tl) = x" `means` [ DefVal "f" Nothing [ Case [Constructor "Cons" [ Hole "hd"
                                                                         , Hole "tl"]]
                                                (Var "x") ]]
    "data Nat = O | S Nat" `means` [ DefData "Nat" [ Variant "O" []
                                                   , Variant "S" [T "Nat"]]]
    "data NatList = Nil | Cons Nat NatList" `means` [ DefData "NatList"
                                                      [ Variant "Nil" []
                                                      , Variant "Cons" [T "Nat", T "NatList"]]]
    "f :: Int\nf = x" `means` [ DefVal "f" (Just (T "Int")) [ Case [] (Var "x") ] ]
    "f :: A -> B\nf = x" `means` [ DefVal "f" (Just (F (T "A") (T "B"))) [ Case [] (Var "x") ] ]


