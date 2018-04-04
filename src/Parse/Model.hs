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
build ValidatedModel{..} =
  let res = map (buildState res) validatedStates in
    reachability $ Model res
  where buildState model ParsedState{..} = State {
            stateId   = parsedId
          , stateInit = parsedInit
          , stateNext = map (getStateById (Model model)) parsedNext
          , statePrev = filter (isJust ∘ find ((≡ parsedId) ∘ stateId) ∘ stateNext) model
          , stateSucc = error "uninitialized field"
          , statePred = error "uninitialized field"
          }

reachability ∷ Model → Model
reachability (Model m) = Model $ map (\s → s{stateSucc = successors s, statePred = predecessors s}) m

-- TODO
-- Ensure uniqueness of state ids, that state ids referenced exists,
-- and move initialness of a state from the global list to specific states.
validate ∷ ParsedModel → ValidatedModel
validate ParsedModel{..} = ValidatedModel
  { validatedStates = map (\s → s { parsedInit = (parsedId s) `elem` parsedInits }) parsedStates
  }
