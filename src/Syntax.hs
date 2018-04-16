module Syntax where

import           Data.List.Unicode ((⧺))
import           Types


-- Given the number of states in the model, estimate how many entries will be saved in the lookup table
numEntries ∷ Int → Form → Int
numEntries _ Truth           = 0
numEntries _ (Not _)         = 0
numEntries _ (And _ _)       = 0
numEntries _ (Nom _)         = 0
numEntries _ (Var _)         = 0
numEntries _ (At (Left _) _) = 0
numEntries n x = n + case x of
  Next ϕ         → num ϕ
  Until ϕ ψ      → num ϕ + num ψ
  At (Right _) ϕ → n + num ϕ
  Bind _ ϕ       → n + num ϕ
  Exists _ ϕ     → n + num ϕ
  where num = numEntries n

-- Perform binding in Form α, VarId x with StateId n, returning the new Form.
bind ∷ Form → VarId → StateId → Form
bind α@(Var x') x n
  | x == x'   = Nom n
  | otherwise = α
bind (At (Left s) ϕ) x n = At (Left s) (bind ϕ x n)
bind α@(At (Right x') ϕ) x n
  | x == x'   = At (Left n) (bind ϕ x n)
  | otherwise = At (Right x') (bind ϕ x n)
bind (Bind x' ϕ) x n
  | x == x'   = bind ϕ x n
  | otherwise = Bind x' (bind ϕ x n)
bind (Exists x' ϕ) x n
  | x == x'   = bind ϕ x n
  | otherwise = Exists x' (bind ϕ x n)
bind α x n = case α of
  Truth     → Truth
  Not ϕ     → Not (bind ϕ x n)
  And ϕ ψ   → And (bind ϕ x n) (bind ψ x n)
  Next ϕ    → Next (bind ϕ x n)
  Until ϕ ψ → Until (bind ϕ x n) (bind ψ x n)
  Nom m     → Nom m
