{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module ModelCheck.Simple where

import           Control.Monad         (forM, liftM2)
import           Control.Monad.Extra   (ifM)
import           Data.Function.Unicode
import           Monad.ModelCheck
import           Prelude               hiding (lookup)
import           Semantics
import           Syntax
import           Tree
import           Types


-- Simple top-down, inductive approach.
check ∷ Model → State → Form → ModelCheck ARead Bool
check m s@State{..} ϕ = lookup s ϕ >>= maybe (check' >>= put ∘ (s, ϕ, )) return
  where
    check' ∷ ModelCheck ARead Bool
    check' = case ϕ of
      Truth          → return True
      Not ϕ          → not <$> check m s ϕ
      And ϕ ψ        → liftM2 (&&) (check m s ϕ) (check m s ψ)
      Future ϕ       → or <$> forM stateSucc (\s' → check m s' ϕ)
      α@(Until ϕ ψ)  → ifM (check m s ψ)
                         (return True) $
                         liftM2 (&&) (check m s ϕ) (foldMap (\s → check m s α) stateSucc)
      Nom n          → return $ stateId == n
      Var x          → return False -- Unbound variable.
      At (Left n) ϕ  → check m (getStateById m n) ϕ
      At (Right x) ϕ → return False -- Unbound variable.
      Bind x ϕ       → check m s $ subst ϕ x stateId
      Exists n ϕ     → or <$> mapM (\s → check m s ϕ) (unModel m)
