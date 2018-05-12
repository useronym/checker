module Syntax where

import           Types


size ∷ Form → Int
size x = case x of
  Truth     → 0
  Prop _    → 0
  Not ϕ     → 0 + size ϕ
  And ϕ ψ   → 0 + (max (size ϕ) (size ψ))
  Next ϕ    → 0 + size ϕ
  Until ϕ ψ → 1 + (max (size ϕ) (size ψ))
  Nom _     → 0
  Data _    → 0
  Var _     → 0
  At _ ϕ    → 0 + size ϕ
  Bind _ ϕ  → 0 + size ϕ

tetrate ∷ Int → Int → Int
tetrate _ 0 = 1
tetrate k n = k ^ (tetrate k (n-1))
