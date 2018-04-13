module Syntax where

import           Data.List.Unicode ((⧺))
import           Types


subformulae ∷ Form → [Form]
subformulae x = x : case x of
  Truth      → []
  Not ϕ      → sub ϕ
  And ϕ ψ    → sub ϕ ⧺ sub ψ
  Future ϕ   → sub ϕ
  Past ϕ     → sub ϕ
  Until ϕ ψ  → sub ϕ ⧺ sub ψ
  Since ϕ ψ  → sub ϕ ⧺ sub ψ
  Nom _      → []
  Var _      → []
  At _ ϕ     → sub ϕ
  Bind _ ϕ   → sub ϕ
  Exists _ ϕ → sub ϕ
  where sub = subformulae

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
