{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Parse.Model where

import           Control.Applicative   hiding ((<|>))
import           Data.ByteString       (readFile)
import           Data.Eq.Unicode
import           Data.Function.Unicode
import           Data.List             (find)
import           Data.Maybe            (fromJust, isJust)
import           Data.Yaml
import           Prelude               hiding (readFile)
import           Semantics
import           Types


loadModel ∷ FilePath → IO (Either ParseException Model)
loadModel path = readFile path >>= return ∘ (fmap load) ∘ decodeEither'

load ∷ ParsedModel → Model
load = reachability ∘ build ∘ validate

reachability ∷ Model → Model
reachability (Model m) = Model $ map (\s → s{stateSucc = successors s, statePred = predecessors s}) m

-- Tie it all up.
build ∷ ParsedModel → Model
build ParsedModel{..} =
  let res = map (buildState res) parsedStates in
    Model res
  where buildState model ParsedState{..} = let thisId = fromJust parsedId in State {
            stateId   = thisId
          , stateInit = parsedInit
          , stateNext = map (\st → fromJust $ find ((≡ st) ∘ getStateId) model) parsedNext
          , statePrev = filter (isJust ∘ find ((≡ thisId) ∘ stateId) ∘ stateNext) model
          , stateSucc = error "uninitialized field"
          , statePred = error "uninitialized field"
          }

-- Ensure uniqueness of state ids, that state ids referenced exists,
-- and move initialness of a state from the global list to specific states.
validate ∷ ParsedModel → ParsedModel
validate = id
