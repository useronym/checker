{-# LANGUAGE RecordWildCards #-}
module Types.Run where

import           Control.Monad         (join)
import           Data.Function.Unicode
import qualified Data.Vector           as V (Vector, fromList, last, length,
                                             (!), (++))
import           Types.Base
import           Types.Tree


data Run = Run
  { runStates   ∷ V.Vector State
  , runCont     ∷ Tree State
  , runContPath ∷ [Tree State]
  }

instance Show Run where
  show Run{..} = show runStates


lengthV ∷ Run → Int
lengthV Run{..} = V.length runStates

atV ∷ Run → Int → State
atV Run{..} i = runStates V.! i

runs ∷ Tree State → [[Tree State]]
runs n@(Node _ []) = [[n]]
runs n@(Node _ ns) = join (map runs ns) >>= return ∘ (n:)

makeRuns ∷ [Tree State] → Tree State → [Run]
makeRuns path t = map makeRun (runs t)
  where makeRun ∷ [Tree State] → Run
        makeRun xs = let vecRun       = V.fromList (map unNode xs)
                         (p, x:_) = break ((== V.last vecRun) ∘ unNode) (path ++ xs)
                     in Run
                        { runStates   = vecRun
                        , runCont     = x
                        , runContPath = p
                        }
        unNode ∷ Tree a → a
        unNode (Node x _) = x

extend ∷ Run → [Run]
extend r = map (\r' → r' { runStates = (runStates r) V.++ (runStates r') }) $ makeRuns (runContPath r) (runCont r)
