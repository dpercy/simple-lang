{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module WellFormedTypes (
  checkProgram,
  explain,
  testWellFormedTypes,
  ) where


import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec

import Model


{-

http://www.jucs.org/jucs_6_4/ensuring_termination_in_esfp/Telford_A.pdf

1. Only strictly positive occurrences are allowed in the inductive definition of
   types. This means that in the definition of a type, T , say, T may not occur
   within the domain of any function space in the definition of T . For example,
   the following would not be allowed:

       data ilist = C (ilist -> Int)

2. T may not be defined via polymorphic type U where T occurs as an instantiation
   of U. For example, we would not allow rosetrees which can be given
   the following definition:

       data Rosetree a = Leaf a | Node (List (Rosetree a))

   -- The problem here is that when you pass (Rosetree a) to List, we don't know
      whether List is contravariant in its type argument.
      So in general, just as types can't pass themselves to the left-hand-side of
      the "->" type constructor, they can't pass themselves to any type constructor.

3. T may not be defined via a type U which is transitively defined using T.

4. T must have a base case i.e. one with no recursive occurrences of T.


Although these checks like TypeCheck, they're in a separate pass because:
- their purpose is to help prove termination, not to prevent type errors
- improvements to typechecking / type inference can be separate from this
- loosening the definition of well-formed types can be separate from typechecking


-}


testWellFormedTypes :: Spec
testWellFormedTypes = do
  it "1. no contravariant recursion" $ do
    checkProgram [ DefData "Unit" [ Variant "Unit" [] ] ]
      `shouldBe` [ DefData "Unit" [ Variant "Unit" [] ] ]
    checkProgram [ DefData "Unit" [ Variant "Unit" [] ]
                 , DefData "Hungry" [ Variant "MakeHungry" [(F (T "Hungry") (T "Unit"))] ]
                 ]
      `shouldBe` [ DefData "Unit" [ Variant "Unit" [] ]
                 , Error (explain (ContravariantRecursion "Hungry" (Variant "MakeHungry" [(F (T "Hungry") (T "Unit"))])))
                 ]
    checkProgram [ DefData "Unit" [ Variant "Unit" [] ]
                 , DefData "WithSelf" [ Variant "WithSelf" [(F (F (T "WithSelf") (T "Unit"))
                                                           (T "Unit"))]
                                     , Variant "BaseCase" []
                                     ]
                 ]
      `shouldBe` [ DefData "Unit" [ Variant "Unit" [] ]
                 , DefData "WithSelf" [ Variant "WithSelf" [(F (F (T "WithSelf") (T "Unit"))
                                                             (T "Unit"))]
                                      , Variant "BaseCase" []
                                      ]
                 ]
    checkProgram [ DefData "Unit" [ Variant "Unit" [] ]
                 , DefData "WithSelfTaker" [ Variant "WithSelfTaker" [(F (F (F (T "WithSelfTaker")
                                                                   (T "Unit"))
                                                                (T "Unit"))
                                                           (T "Unit"))]
                                     , Variant "BaseCase" []
                                     ]
                 ]
      `shouldBe` [ DefData "Unit" [ Variant "Unit" [] ]
                 , Error (explain (ContravariantRecursion "WithSelfTaker"
                                   (Variant "WithSelfTaker" [(F (F (F (T "WithSelfTaker") (T "Unit"))
                                                                 (T "Unit"))
                                                              (T "Unit"))])))
                 ]
      
  it "2. no using self as a type argument" $ do
    "TODO" `shouldBe` "need to implement polymorphic types first"
    
  it "3. no cyclic definitions" $ do
    checkProgram [ DefData "X" [ Variant "MakeX" [T "Y"] ]
                 , DefData "Y" [ Variant "MakeY" [T "X"] ]
                 ]
      `shouldBe` [ Error (explain (CyclicTypeDefs "X" ["Y"]))
                 , Error (explain (CyclicTypeDefs "Y" ["X"]))
                 ]
    checkProgram [ DefData "A" [ Variant "MakeX" [T "B"] ]
                 , DefData "B" [ Variant "MakeY" [T "C"] ]
                 , DefData "C" [ Variant "MakeY" [T "A"] ]
                 ]
      `shouldBe` [ Error (explain (CyclicTypeDefs "A" ["B", "C"]))
                 , Error (explain (CyclicTypeDefs "B" ["C", "A"]))
                 , Error (explain (CyclicTypeDefs "C" ["A", "B"]))
                 ]
  it "4. base case is required" $ do
    checkProgram [ DefData "Nat" [ Variant "Succ" [ T "Nat" ] ] ]
      `shouldBe` [ Error (explain (MissingBaseCase "Nat")) ]

  it "good example: nat" $ do
    checkProgram [ DefData "Nat" [ Variant "Zero" []
                                 , Variant "Succ" [ T "Nat" ]
                                 ] ]
      `shouldBe` [ DefData "Nat" [ Variant "Zero" []
                                 , Variant "Succ" [ T "Nat" ]
                                 ] ]


data MalformedTypeError = ContravariantRecursion { typeName :: Uppercase
                                                 , variant :: Variant
                                                 }
                        | CyclicTypeDefs { typeName :: Uppercase, typeNames :: [Uppercase] }
                        | MissingBaseCase { typeName :: Uppercase }
                        deriving (Show, Eq)

explain :: MalformedTypeError -> String
explain (ContravariantRecursion { typeName, variant }) =
  "Type `" ++ typeName ++ "` refers to itself in a contravariant position in " ++ show variant
explain (CyclicTypeDefs { typeName, typeNames }) =
  "Type `" ++ typeName ++ "` is defined in terms of itself; the cycle is " ++ show typeNames
explain (MissingBaseCase { typeName }) =
  "Type `" ++ typeName ++ "` has no base case"

checkProgram :: Program -> Program
checkProgram prog =
  let prog2 = map checkContravariantRecursion prog
      prog3 = map checkBaseCase prog2
      prog4 = map (checkCyclicDefs (getTypedefGraph prog3)) prog3
  in prog4

recover :: Stmt -> Either MalformedTypeError () -> Stmt
recover _ (Left err) = Error (explain err)
recover s (Right ()) = s

checkContravariantRecursion :: Stmt -> Stmt
checkContravariantRecursion s@(Error{}) = s
checkContravariantRecursion s@(DefVal{}) = s
checkContravariantRecursion s@(Expr _) = s
checkContravariantRecursion s@(DefData tname variants) =
  recover s (mapM_ (checkContra tname) variants)

checkContra :: Uppercase -> Variant -> Either MalformedTypeError ()
checkContra tname v@(Variant _ types) = if any (tyContainsContra tname) types
                                        then Left (ContravariantRecursion { typeName = tname
                                                                          , variant = v
                                                                          })
                                        else return ()

tyContainsContra :: Uppercase -> Type -> Bool
tyContainsContra _ (T _) = False
tyContainsContra tname (F inn out) = tyContainsCov tname inn || tyContainsContra tname out

tyContainsCov :: Uppercase -> Type -> Bool
tyContainsCov tname (T t) = tname == t
tyContainsCov tname (F inn out) = tyContainsContra tname inn || tyContainsCov tname out


type CycleEnv = Map Uppercase [Uppercase]

-- the map tells you: this type is defined in terms of these other types.
checkCyclicDefs :: CycleEnv -> Stmt -> Stmt
checkCyclicDefs env s@(DefData tyName _) = recover s (checkCycle env tyName)
checkCyclicDefs _   s@(Expr{}) = s
checkCyclicDefs _   s@(Error{}) = s
checkCyclicDefs _   s@(DefVal{}) = s

checkCycle :: CycleEnv -> Uppercase -> Either MalformedTypeError ()
checkCycle env tyName = checkCyclePrefix env [] tyName

-- checks whether tyName forms a non-self cycle
checkCyclePrefix :: CycleEnv -> [Uppercase] -> Uppercase -> Either MalformedTypeError ()
checkCyclePrefix env seen tyName =
  if tyName `elem` seen
  then Left (CyclicTypeDefs
             tyName
             (reverse (takeWhile (/= tyName) seen)))
  else case Map.lookup tyName env of
        Nothing -> return ()
        Just children -> mapM_ (checkCyclePrefix env (tyName:seen))
                         -- exclude self from children to avoid self-loops
                         (filter (/= tyName) children)

getTypedefGraph :: Program -> CycleEnv
getTypedefGraph defs = Map.unionsWith (++) (map tdGraphStmt defs)

tdGraphStmt :: Stmt -> CycleEnv
tdGraphStmt (Error{}) = Map.empty
tdGraphStmt (DefVal{}) = Map.empty
tdGraphStmt (Expr _) = Map.empty
tdGraphStmt (DefData tname variants) = Map.unionsWith (++) (map (tdGraphVariant tname) variants)

tdGraphVariant :: Uppercase -> Variant -> CycleEnv
tdGraphVariant tname (Variant _ argtypes) = Map.unionsWith (++) (map (tdGraphTy tname) argtypes)

tdGraphTy :: Uppercase -> Type -> CycleEnv
tdGraphTy tname (T t) = Map.fromList [(tname, [t])]
tdGraphTy tname (F inn out) = Map.unionWith (++) (tdGraphTy tname inn) (tdGraphTy tname out)


-- Every data definition must include a base case.
-- Otherwise it's impossible to actually construct a value of that type.
checkBaseCase :: Stmt -> Stmt
checkBaseCase s@(Expr _) = s
checkBaseCase s@(Error{}) = s
checkBaseCase s@(DefVal{}) = s
checkBaseCase s@(DefData tname variants) = if any (not . variantMentions tname) variants
                                           then s
                                           else Error (explain (MissingBaseCase tname))

variantMentions :: Uppercase -> Variant -> Bool
variantMentions tname (Variant _ args) = any (typeMentions tname) args

typeMentions :: Uppercase -> Type -> Bool
typeMentions tname (T t) = tname == t
typeMentions tname (F inn out) = typeMentions tname inn || typeMentions tname out

