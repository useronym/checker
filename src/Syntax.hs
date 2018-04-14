module Syntax where

import           Data.List.Unicode ((⧺))
import           Types


-- Given the number of states in the model, how many model-checks will we need to perform?
numEntries ∷ Int → Form → Int
numEntries n x = case x of
  Truth          → 0
  Not ϕ          → 0
  And ϕ ψ        → 0
  Future ϕ       → 1 + num ϕ
  Past ϕ         → 1 + num ϕ
  Until ϕ ψ      → 1 + num ϕ + num ψ
  Since ϕ ψ      → 1 + num ϕ + num ψ
  Nom _          → 0
  Var _          → 0
  At (Left _) ϕ  → 1 + num ϕ
  At (Right _) ϕ → n * (1 + num ϕ)
  Bind _ ϕ       → n * (1 + num ϕ)
  Exists _ ϕ     → n * (1 + num ϕ)
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
  Future ϕ  → Future (bind ϕ x n)
  Past ϕ    → Past (bind ϕ x n)
  Until ϕ ψ → Until (bind ϕ x n) (bind ψ x n)
  Since ϕ ψ → Since (bind ϕ x n) (bind ψ x n)
  Nom m     → Nom m
