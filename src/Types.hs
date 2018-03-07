{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Types where

import           Control.Applicative.Unicode
import           Control.Distributed.Process.Serializable
import           Data.Binary                              (Binary)
import           Data.Function.Unicode
import           Data.Hashable                            (Hashable (..))
import           Data.List                                (intercalate)
import           Data.List.Unicode                        ((⧺))
import           Data.Maybe                               (isJust)
import           Data.Yaml
import           GHC.Generics                             (Generic)


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
    stateId   ∷ StateId -- ^ A unique identifier.
  , stateInit ∷ Bool    -- ^ Initial?
  , stateNext ∷ [State] -- ^ List of directly reachable states.
  , statePrev ∷ [State] -- ^ List of direct predecessor states.
  , stateSucc ∷ [State] -- ^ List of all reachable states. Finite.
  , statePred ∷ [State] -- ^ List of all states this state can be reached from. Finite.
  } deriving (Generic)

instance Show State where
  show State{..} = showInit ⧺ "[" ⧺ stateId ⧺ "] " ⧺ intercalate " " [showNext, showPrev, showSucc, showPred]
    where showInit   = if stateInit then "→" else " "
          showNext   = "Next: " ⧺ showStates stateNext
          showPrev   = "Prev: " ⧺ showStates statePrev
          showSucc   = "Succ: " ⧺ showStates stateSucc
          showPred   = "Pred: " ⧺ showStates statePred
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

instance Binary State



-- The model, then, is a list of such states.
newtype Model = Model [State]

unModel ∷ Model → [State]
unModel (Model m) = m

instance Show Model where
  show (Model xs) = unlines $ map show xs


-- We also have structure which is parsed from file and later converted to the "real" thing.
data PolyState a = PolyState {
    polyId   ∷ a             -- ^ Identifier.
  , polyInit ∷ Bool          -- ^ Initial?
  , polyNext ∷ [StateId]     -- ^ List of directly reachable states.
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (PolyState a) where
  parseJSON (Object o) = PolyState <$>
      o .: "id"
    ⊛ o .:? "init" .!= False
    ⊛ o .:? "next" .!= []

instance Binary a => Binary (PolyState a)
instance Serializable a => Serializable (PolyState a)

type ParsedState = PolyState (Maybe StateId)
type ValidatedState = PolyState StateId

-- We allow specifying initial states both as a list and separately in each state.
data PolyModel a = PolyModel {
    polyStates ∷ [PolyState a]
  , polyInits  ∷ [StateId]
  } deriving (Generic)

instance FromJSON a => FromJSON (PolyModel a) where
  parseJSON (Object o) = PolyModel <$>
      o .: "states"
    ⊛ o .:? "initial" .!= []

instance Binary a => Binary (PolyModel a)
instance Serializable a => Serializable (PolyModel a)

type ParsedModel = PolyModel (Maybe StateId)
type ValidatedModel = PolyModel StateId
