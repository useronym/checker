{-# LANGUAGE RecordWildCards #-}
module Parse.Model (parse, build) where

import           Data.ByteString       (readFile)
import           Data.Eq.Unicode
import           Data.Function.Unicode
import           Data.List             (find)
import           Data.Maybe            (isJust)
import           Data.Yaml
import           Prelude               hiding (readFile)
import           Semantics
import           Types


parse ∷ FilePath → IO (Either ParseException ValidatedModel)
parse path = readFile path >>= return ∘ fmap validate ∘ decodeEither'

-- Tie it all up.
build ∷ ValidatedModel → Model
build ValidatedModel{..} =
  let model = map (buildState model) validatedStates in
    Model model
  where buildState model ParsedState{..} = let s = State {
            stateId    = parsedId
          , stateInit  = parsedInit
          , stateNext  = map (getStateById (Model model)) parsedNext
          , statePrev  = filter (isJust ∘ find ((≡ parsedId) ∘ stateId) ∘ stateNext) model
          , stateReach = successors s
          } in s

validate ∷ ParsedModel → ValidatedModel
validate ParsedModel{..} = ValidatedModel
  { validatedStates = map (\s → s { parsedInit = (parsedId s) `elem` parsedInits }) parsedStates
  }
