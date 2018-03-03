module Semantics where

import           Data.Function.Unicode
import           Data.List             (find)
import           Data.Maybe            (fromJust)
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


-- Unsafe.
getStateById ∷ Model → StateId → State
getStateById m s = fromJust $ lookupStateById m s

lookupStateById ∷ Model → StateId → Maybe State
lookupStateById (Model m) s = find ((== s) ∘ getStateId) m
