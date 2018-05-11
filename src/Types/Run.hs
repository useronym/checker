{-# LANGUAGE RecordWildCards #-}
module Types.Run where

import           Control.Arrow         (first)
import           Control.Monad         (join)
import           Data.Function.Unicode
import           Data.Maybe            (mapMaybe)
import qualified Data.Vector           as V (Vector, fromList, last, length,
                                             (!), (++))
import           Types.Base
import           Types.Tree


data Run = Run
  { runStates   ∷ V.Vector State
  , runCont     ∷ Maybe (Tree State)
  , runContPref ∷ [Tree State]
  }

instance Show Run where
  show Run{..} = show runStates


lengthV ∷ Run → Int
lengthV Run{..} = V.length runStates

atV ∷ Run → Int → State
atV Run{..} i = runStates V.! i

runs ∷ Tree State → [([Tree State], Bool)]
runs n@(Jump _)    = [([n], True)]
runs n@(Node _ []) = [([n], False)]
runs n@(Node _ ns) = join (map runs ns) >>= return ∘ (first (n:))

makeRuns ∷ [Tree State] → Tree State → [Run]
makeRuns pref t = map makeRun (runs t)
  where makeRun ∷ ([Tree State], Bool) → Run
        makeRun (xs, isLoop) = let vecRun   = V.fromList (mapMaybe unTreeM xs)
                                   (p, x:_) = break ((== unTree (last xs)) ∘ unTree) (pref ++ xs)
          in Run
             { runStates   = vecRun
             , runCont     = if isLoop then Just x else Nothing
             , runContPref = p
             }
        unTree (Jump x)   = x
        unTree (Node x _) = x
        unTreeM (Jump _)   = Nothing
        unTreeM (Node x _) = Just x

extend ∷ Run → [Run]
extend r@Run{runCont = Nothing} = [r]
extend r@Run{runCont = Just c}  = for (makeRuns (runContPref r) c)
                                   (\r' → r' { runStates = (runStates r) V.++ (runStates r') })
  where for = flip map
