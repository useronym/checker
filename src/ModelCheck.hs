{-# LANGUAGE TupleSections #-}
module ModelCheck where

import           Control.Arrow         ((***))
import           Data.Function.Unicode
--import           Monad.ModelCheck
import           Types


--check ∷ Model → State → Form → ModelCheck ARead Bool
--check m s ϕ = checkRuns stateRuns >>= put ∘ (s, )

checkRuns ∷ Form → Int → Env → Bool → [Run] → [(Run, Bool)]
checkRuns ϕ i env c rs = concat $ map (checkRun ϕ i env c) rs

checkRun ∷ Form → Int → Env → Bool → Run → [(Run, Bool)]
checkRun ϕ i env c [] = if c then checkRuns ϕ i env (extend r) else [(r, False)]
checkRun x i env c r = case x of
  Truth     → [(r, True)]
  Nom n     → [(r, state == n)]
  Var x     → [(r, fromMaybe False $ (i==) <$> lookup x env)]
  Not ϕ     → (id *** not) <$> checkRun ϕ i env True r
  And ϕ ψ   →
    case unzip $ checkRun ϕ i env True r of
      (rs, ress) | and ress → checkRuns ψ i env True rs
      (rs, ress) → zip rs ress
  Next ϕ    → checkRun ϕ (i+1) env True r
  Until ϕ ψ →
    case unzip $ checkRun ψ i env True r of
      (rs, ress) | and ress → (rs, True)
      (rs, _) →
        case unzip $ checkRuns ϕ i env True rs of
          (rs', ress) | and ress → checkRuns (ϕ `Until` ψ) env (i+1) c rs'
          (rs', ress) → zip rs' ress
  At x ϕ    →
    case lookup x env of
      Just i' → checkRun ϕ i' env True r
      Nothing → [(r, False)]
  Bind x ϕ  → checkRun ϕ i ((x,i):env) True r
