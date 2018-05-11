{-# LANGUAGE TupleSections #-}
module ModelCheck (check) where

import           Data.Maybe            (fromMaybe)
import           Control.Arrow         (second)
import           Monad.ModelCheck
import           Types


type Env = [(VarId, Int)]

check ∷ Form → Run → ModelCheck ARead ()
check ϕ r = putMany (checkRun ϕ 0 [] False r )

checkRuns ∷ Form → Int → Env → Bool → [Run] → [(Run, Bool)]
checkRuns ϕ i env c rs = concat $ map (checkRun ϕ i env c) rs

checkRun ∷ Form → Int → Env → Bool → Run → [(Run, Bool)]
checkRun x i env c r
  | (lengthV r) <= i = if c then checkRuns x i env False (extend r) else [(r, False)]
  | otherwise = case x of
  Truth     → [(r, True)]
  Nom n     → [(r, stateId (r `atV` i) == n)]
  Data x    → [(r, fromMaybe False $
                 lookup x env >>= \i' → return $ stateData (r `atV` i) == stateData (r `atV` i'))]
  Var x     → [(r, fromMaybe False $ (i==) <$> lookup x env)]
  Not ϕ     → (second not) <$> checkRun ϕ i env True r
  And ϕ ψ   →
    case unzip $ checkRun ϕ i env True r of
      (rs, ress) | and ress → checkRuns ψ i env True rs
      (rs, ress) → zip rs ress
  Next ϕ    → checkRun ϕ (i+1) env True r
  Until ϕ ψ →
    case unzip $ checkRun ψ i env True r of
      (rs, ress) | and ress → zip rs ress
      (rs, _) →
        case unzip $ checkRuns ϕ i env True rs of
          (rs', ress) | and ress → checkRuns (Until ϕ ψ) (i+1) env c rs'
          (rs', ress) → zip rs' ress
  At x ϕ    →
    case lookup x env of
      Just i' → checkRun ϕ i' env True r
      Nothing → [(r, False)]
  Bind x ϕ  → checkRun ϕ i ((x,i):env) True r
