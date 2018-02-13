{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Types where

import           Control.Applicative.Unicode
import           Data.Function.Unicode
import           Data.List                   (intercalate)
import           Data.List.Unicode           ((⧺))
import           Data.Maybe                  (isJust)
import           Data.Yaml


-- The data type of formulae.
type VarId = Char
type StateId = String

data Form where
  Truth  ∷ Form
  Not    ∷ Form → Form
  And    ∷ Form → Form → Form
  Future ∷ Form → Form
  Past   ∷ Form → Form
  Until  ∷ Form → Form → Form
  Since  ∷ Form → Form → Form
  Nom    ∷ StateId → Form
  Var    ∷ VarId → Form
  At     ∷ (Either StateId VarId) → Form → Form
  Bind   ∷ VarId → Form → Form
  Exists ∷ VarId → Form → Form
    deriving (Show)


-- A state in a hybrid Kripke structure.
data State = State {
    stateId   ∷ StateId -- ^ A unique identifier.
  , stateInit ∷ Bool    -- ^ Initial?
  , stateNext ∷ [State] -- ^ List of directly reachable states.
  , statePrev ∷ [State] -- ^ List of direct predecesor states.
  }

instance Show State where
  show State{..} = showInit ++ "[" ++ stateId ++ "] " ++ showNext
    where showInit   = if stateInit then "→" else " "
          showNext   = "Next: " ⧺ (intercalate "," $ map getStateId stateNext)

-- Eh.
getStateId = stateId

-- The model, then, is a list of such states.
newtype Model = Model [State]

instance Show Model where
  show (Model xs) = unlines $ map show xs


-- We also have structure which is parsed from file and later converted to the "real" thing.
data ParsedState = ParsedState {
    parsedId   ∷ Maybe StateId -- ^ A unique identifier.
  , parsedInit ∷ Bool          -- ^ Initial?
  , parsedNext ∷ [StateId]     -- ^ List of directly reachable states.
  } deriving (Show)

instance FromJSON ParsedState where
  parseJSON (Object o) = ParsedState <$>
      o .: "id"
    ⊛ o .:? "init" .!= False
    ⊛ o .:? "next" .!= []

-- We allow specifying initial states both as a list and separately in each state.
data ParsedModel = ParsedModel {
    parsedStates ∷ [ParsedState]
  , parsedInits  ∷ [StateId]
  }

instance FromJSON ParsedModel where
  parseJSON (Object o) = ParsedModel <$>
      o .: "states"
    ⊛ o .:? "initial" .!= []
