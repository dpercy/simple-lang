module Util where

import Model

converge :: Eq a => (a -> a) -> a -> a
converge step start = if next == start then start else converge step next
  where next = step start

class Explain a where
  explain :: a -> String


recover :: Explain err => Stmt -> Either err () -> Stmt
recover _ (Left err) = Error (explain err)
recover s (Right ()) = s
