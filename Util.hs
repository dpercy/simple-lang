module Util where


converge :: Eq a => (a -> a) -> a -> a
converge step start = if next == start then start else converge step next
  where next = step start
