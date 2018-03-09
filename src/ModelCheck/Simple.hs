{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module ModelCheck.Simple where

import           Control.Monad         (liftM2, forM)
import           Control.Monad.Extra   (ifM)
import           Data.Function.Unicode
import qualified Monad.ModelCheck      as M
import           Semantics
import           Syntax
import           Types


-- Simple top-down, inductive approach.
check ∷ Model → State → Form → M.ModelCheck Bool
check m s@State{..} ϕ = M.lookup s ϕ >>= maybe (check' >>= M.put ∘ (s, ϕ, )) return
  where
    check' ∷ M.ModelCheck Bool
    check' = case ϕ of
      Truth          → return True
      Not ϕ          → not <$> check m s ϕ
      And ϕ ψ        → liftM2 (&&) (check m s ϕ) (check m s ψ)
      Future ϕ       → or <$> forM stateSucc (\s' → check m s' ϕ)
      α@(Until ϕ ψ)  → ifM (check m s ψ) --TODO: fix
                         (return True) $
                         liftM2 (&&) (check m s ϕ) (and <$> mapM (\s → check m s α) stateNext)
      Nom n          → return $ stateId == n
      Var x          → return False -- Unbound variable.
      At (Left n) ϕ  → check m (getStateById m n) ϕ
      At (Right x) ϕ → return False -- Unbound variable.
      Bind x ϕ       → check m s $ subst ϕ x stateId
      Exists n ϕ     → or <$> mapM (\s → check m s ϕ) (unModel m)
