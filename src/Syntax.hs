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
