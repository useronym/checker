{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Types where

import           Control.Applicative.Unicode
import           Control.Distributed.Process.Serializable
import           Data.Binary                              (Binary, get, put)
import           Data.Function.Unicode
import           Data.Hashable                            (Hashable (..))
import           Data.List                                (intercalate)
import           Data.List.Unicode                        ((⧺))
import           Data.Maybe                               (isJust)
import           Data.Yaml
import           GHC.Generics                             (Generic)
import           Tree


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
    deriving (Show, Eq, Ord, Generic)

instance Hashable Form
instance Binary Form
instance Serializable Form


-- A state in a hybrid Kripke structure.
data State = State {
    stateId   ∷ StateId    -- ^ A unique identifier.
  , stateInit ∷ Bool       -- ^ Initial?
  , stateNext ∷ [State]    -- ^ List of directly reachable states.
  , statePrev ∷ [State]    -- ^ List of direct predecessor states.
  , stateSucc ∷ Tree State -- ^ List of all reachable states. Finite.
  , statePred ∷ Tree State -- ^ List of all states this state can be reached from. Finite.
  }

instance Show State where
  show State{..} = showInit ⧺ "[" ⧺ stateId ⧺ "] " ⧺ intercalate " " [showNext, showPrev]
    where showInit   = if stateInit then "→" else " "
          showNext   = "Next: " ⧺ showStates stateNext
          showPrev   = "Prev: " ⧺ showStates statePrev
          showStates = enclose ∘ (intercalate ",") ∘ (map getStateId)
          enclose x  = "[" ⧺ x ⧺ "]"

-- Eh.
getStateId = stateId

instance Eq State where
  (==) a b = (stateId a) == (stateId b)

instance Ord State where
  (<=) a b = (stateId a) <= (stateId b)

instance Hashable State where
  hashWithSalt s State{..} = hashWithSalt s stateId


-- The model, then, is a list of such states.
newtype Model = Model [State]

unModel ∷ Model → [State]
unModel (Model m) = m

instance Show Model where
  show (Model xs) = unlines $ map show xs


-- We also have structure which is parsed from file and later converted to the "real" thing.
data ParsedState = ParsedState {
    parsedId   ∷ StateId       -- ^ Identifier.
  , parsedInit ∷ Bool          -- ^ Initial?
  , parsedNext ∷ [StateId]     -- ^ List of directly reachable states.
  } deriving (Show, Generic)

instance FromJSON ParsedState where
  parseJSON (Object o) = ParsedState <$>
      o .: "id"
    ⊛ o .:? "init" .!= False
    ⊛ o .:? "next" .!= []

instance Binary ParsedState
instance Serializable ParsedState

-- We allow specifying initial states both as a list and separately in each state.
data ParsedModel = ParsedModel {
    parsedStates ∷ [ParsedState]
  , parsedInits  ∷ [StateId]
  }

instance FromJSON ParsedModel where
  parseJSON (Object o) = ParsedModel <$>
      o .: "states"
    ⊛ o .:? "initial" .!= []

data ValidatedModel = ValidatedModel {
    validatedStates ∷ [ParsedState]
  } deriving (Generic)

instance Binary ValidatedModel
instance Serializable ValidatedModel
