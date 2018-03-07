{-# LANGUAGE RecordWildCards #-}
module Parse.Model (parse, build) where

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


parse ∷ FilePath → IO (Either ParseException ValidatedModel)
parse path = readFile path >>= return ∘ fmap validate ∘ decodeEither'

-- Tie it all up.
build ∷ ValidatedModel → Model
build PolyModel{..} =
  let res = map (buildState res) polyStates in
    reachability $ Model res
  where buildState model PolyState{..} = State {
            stateId   = polyId
          , stateInit = polyInit
          , stateNext = map (getStateById (Model model)) polyNext
          , statePrev = filter (isJust ∘ find ((≡ polyId) ∘ stateId) ∘ stateNext) model
          , stateSucc = error "uninitialized field"
          , statePred = error "uninitialized field"
          }

reachability ∷ Model → Model
reachability (Model m) = Model $ map (\s → s{stateSucc = successors s, statePred = predecessors s}) m

-- TODO
-- Ensure uniqueness of state ids, that state ids referenced exists,
-- and move initialness of a state from the global list to specific states.
validate ∷ ParsedModel → ValidatedModel
validate PolyModel{..} = PolyModel
  { polyStates = map (\p@PolyState{..} → p{ polyId = fromJust polyId }) polyStates
  , polyInits = []
  }
