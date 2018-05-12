{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Types.Base where

import           Control.Applicative.Unicode
import           Control.Distributed.Process.Serializable
import           Control.Monad                            (liftM2)
import           Data.Binary                              (Binary)
import           Data.Function.Unicode
import           Data.Hashable                            (Hashable (..))
import           Data.List                                (intercalate)
import           Data.Yaml
import           GHC.Generics                             (Generic)
import           Types.Tree


-- The data type of formulae.
type VarId = Char
type StateId = String
type PropId = String

data Form where
  Truth  ∷ Form
  Prop   ∷ PropId → Form
  Not    ∷ Form → Form
  And    ∷ Form → Form → Form
  Next   ∷ Form → Form
  Until  ∷ Form → Form → Form
  Nom    ∷ StateId → Form
  Data   ∷ VarId → Form
  Var    ∷ VarId → Form
  At     ∷ VarId → Form → Form
  Bind   ∷ VarId → Form → Form
    deriving (Eq, Ord, Generic)

boolOr ∷ Form → Form → Form
boolOr ϕ ψ = Not ((Not ϕ) `And` (Not ψ))

boolImplies ∷ Form → Form → Form
boolImplies ϕ ψ = Not (ϕ `And` (Not ψ))

future ∷ Form → Form
future ϕ = Truth `Until` ϕ

globally ∷ Form → Form
globally ϕ = Not (future (Not ϕ))

instance Show Form where
  show f = let str = case f of
                 (Truth `Until` ϕ)             → "F" ++ show ϕ
                 (Not (Truth `Until` (Not ϕ))) → "G" ++ show ϕ
                 Truth                         → "⊤"
                 Prop p                        → p
                 Not ϕ                         → "¬" ++ show ϕ
                 And ϕ ψ                       → show ϕ ++ " ∧ " ++ show ψ
                 Next ϕ                        → "X" ++ show ϕ
                 Until ϕ ψ                     → show ϕ ++ " U " ++ show ψ
                 Nom n                         → show n
                 Data x                        → "~" ++ [x]
                 Var x                         → [x]
                 At x ϕ                        → "@" ++ [x] ++ "." ++ show ϕ
                 Bind x ϕ                      → "↓" ++ [x] ++ "." ++ show ϕ
      in if isDeep f then enclose str else str
    where isDeep f = case f of
            Truth  → False
            Not ϕ  → isDeep ϕ
            Next ϕ → isDeep ϕ
            Nom _  → False
            Data _ → False
            Var _  → False
            _      → True
          enclose = (++")") ∘ ("("++)

instance Hashable Form
instance Binary Form
instance Serializable Form


-- A state in a hybrid Kripke structure.
data State = State {
    stateId    ∷ StateId    -- ^ A unique identifier.
  , stateInit  ∷ Bool       -- ^ Initial?
  , stateProps ∷ [PropId]   -- ^ List of atomic propositions satisfied.
  , stateData  ∷ String     -- ^ Data word assigned to this state.
  , stateNext  ∷ [State]    -- ^ List of directly reachable states.
  , statePrev  ∷ [State]    -- ^ List of direct predecessor states.
  , stateReach ∷ Tree State -- ^ Reachability tree.
  }

instance Show State where
  show State{..} = stateId

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
    parsedId    ∷ StateId       -- ^ Identifier.
  , parsedInit  ∷ Bool          -- ^ Initial?
  , parsedProps ∷ [PropId]      -- ^ List of atomic propositions satisfied.
  , parsedData  ∷ String        -- ^ Data word assigned to this state.
  , parsedNext  ∷ [StateId]     -- ^ List of directly reachable states.
  } deriving (Show, Generic)

instance FromJSON ParsedState where
  parseJSON (Object o) = ParsedState <$>
      o .: "id"
    ⊛ o .:? "init" .!= False
    ⊛ o .:? "props" .!= []
    ⊛ o .:? "data" .!= ""
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


data SerRun = SerRun [StateId]
  deriving (Eq, Ord, Generic)

instance Show SerRun where
  show (SerRun xs) = intercalate " " xs

instance Hashable SerRun
instance Binary SerRun
instance Serializable SerRun
