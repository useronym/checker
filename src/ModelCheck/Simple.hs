{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module ModelCheck.Simple where

import           Control.Monad         (forM, liftM2)
import           Control.Monad.Extra   (ifM)
import           Data.Function.Unicode
import           Monad.ModelCheck
import           Prelude               hiding (lookup)
import           Syntax
import           Tree
import           Types


-- Simple top-down, inductive approach.
check ∷ Run → State → Form → ModelCheck ARead Bool
check _ _ Truth            = return True
check m s (Not ϕ)          = not <$> check m s ϕ
check m s (And ϕ ψ)        = liftM2 (&&) (check m s ϕ) (check m s ψ)
check m s (Nom n)          = return $ (stateId s) == n
check m s (Var _)          = return False
check m s (At (Right _) _) = return False
check m s@State{..} ϕ      = lookup s ϕ >>= maybe (check' >>= put ∘ (s, ϕ, )) return
  where
    check' ∷ ModelCheck ARead Bool
    check' = case ϕ of
      Next ϕ        → or <$> mapM (\s' → check m s' ϕ) stateNext
      Until ϕ ψ     → fmap threeToBool $ flip foldMap (getPointedRun m s) $ \s' → caseM
                        [(check m s' ψ, Yes)
                        ,(check m s' ϕ, Maybe)]
      At (Left n) ϕ → check m (getStateById m n) ϕ
      Bind x ϕ      → let ϕ' = bind ϕ x stateId in check m s ϕ'
