{-# LANGUAGE UnicodeSyntax #-}
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

-- Substitute in Form α, replacing occurences of VarId x with StateId n, returning the new Form.
subst ∷ Form → VarId → StateId → Form
subst α@(Var x') x n
  | x == x'   = Nom n
  | otherwise = α
subst (At (Left s) ϕ) x n = At (Left s) (subst ϕ x n)
subst α@(At (Right x') ϕ) x n
  | x == x'   = At (Left n) (subst ϕ x n)
  | otherwise = At (Right x') (subst ϕ x n)
subst α@(Bind x' ϕ) x n
  | x == x'   = α -- Shadowing.
  | otherwise = Bind x' (subst ϕ x n)
subst α@(Exists x' ϕ) x n
  | x == x'   = α -- Shadowing.
  | otherwise = Exists x' (subst ϕ x n)
subst α x n = case α of
  Not ϕ     → Not (subst ϕ x n)
  And ϕ ψ   → And (subst ϕ x n) (subst ψ x n)
  Future ϕ  → Future (subst ϕ x n)
  Past ϕ    → Past (subst ϕ x n)
  Until ϕ ψ → Until (subst ϕ x n) (subst ψ x n)
  Since ϕ ψ → Since (subst ϕ x n) (subst ψ x n)
  ϕ         → ϕ -- Truth and Nom.
