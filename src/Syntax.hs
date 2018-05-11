module Syntax where

import           Types


size ∷ Form → Int
size x = case x of
  Truth     → 1
  Not ϕ     → 1 + size ϕ
  And ϕ ψ   → 1 + (max (size ϕ) (size ψ))
  Next ϕ    → 1 + size ϕ
  Until ϕ ψ → 1 + (max (size ϕ) (size ψ))
  Nom _     → 1
  Data _    → 1
  Var _     → 1
  At _ ϕ    → 1 + size ϕ
  Bind _ ϕ  → 1 + size ϕ

tetrate ∷ Int → Int → Int
tetrate _ 0 = 1
tetrate k n = k ^ (tetrate k (n-1))
