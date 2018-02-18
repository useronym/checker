{-# LANGUAGE UnicodeSyntax #-}
module Semantics where

import           Data.Function.Unicode
import           Types


successors ∷ State → [State]
successors = reachableWith stateNext

predecessors ∷ State → [State]
predecessors = reachableWith statePrev

-- Uses DFS; breaks the cycles.
reachableWith ∷ (State → [State]) → State → [State]
reachableWith f = reverse ∘ reachableWith' f []

reachableWith' ∷ (State → [State]) → [State] → State → [State]
reachableWith' f collected x = foldl go collected (f x)
  where go acc state = if state `elem` acc
                         then acc
                         else reachableWith' f (state:acc) state
