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
      Future ϕ       → foldMap
                         (\s' → if s == s' then return False else check m s' ϕ)
                         stateSucc
      α@(Until ϕ ψ)  → threeToBool <$> foldMap
                         (\s' → caseM
                                 [(check m s' ψ, Yes)
                                 ,(check m s' ϕ, Maybe)])
                         stateSucc
      Nom n          → return $ stateId == n
      Var x          → return False -- Unbound variable.
      At (Left n) ϕ  → check m (getStateById m n) ϕ
      At (Right x) ϕ → return False -- Unbound variable.
      Bind x ϕ       → let ϕ' = subst ϕ x stateId in check m s ϕ'
      Exists x ϕ     → or <$> mapM (\s' → let ϕ' = subst ϕ x (getStateId s') in check m s' ϕ') (unModel m)
