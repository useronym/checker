{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}
module ModelCheck.Simple where

import           Semantics
import           Syntax
import           Types


-- Simple top-down, inductive approach.
check ∷ Model → State → Form → Bool
check _ s Truth                   = True
check m s (Not ϕ)                 = not $ check m s ϕ
check m s (And ϕ ψ)               = (check m s ϕ) && (check m s ψ)
check m State{..} α@(Future ϕ)    = and $ map (\s' → if check m s' ϕ then True else check m s' (Future ϕ)) stateNext
check m s@State{..} α@(Until ϕ ψ) = if check m s ψ then True else (check m s ϕ) && (and $ map (\s → check m s α) stateNext)
check _ State{..} (Nom n)         = stateId == n
check _ s (Var x)                 = False -- Unbound variable.
check m s (At (Left n) ϕ)         = check m (getStateById m n) ϕ
check m s (At (Right x) ϕ)        = False -- Unbound variable.
check m s@State{..} (Bind x ϕ)    = check m s $ subst ϕ x stateId
check m _ (Exists n ϕ)            = or $ map (\s → check m s ϕ) (unModel m)
