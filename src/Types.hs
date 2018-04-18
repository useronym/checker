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
import           Data.List                                (find, intercalate)
import           Data.List.Unicode                        ((⧺))
import           Data.Maybe                               (fromJust)
import           Data.Maybe                               (isJust)
import           GHC.Generics                             (Generic)
import           Tree


-- The data type of formulae.
type VarId = Char
data Form where
  Truth  ∷ Form
  Not    ∷ Form → Form
  And    ∷ Form → Form → Form
  Next   ∷ Form → Form
  Until  ∷ Form → Form → Form
  Nom    ∷ StateId → Form
  Var    ∷ VarId → Form
  At     ∷ (Either StateId VarId) → Form → Form
  Bind   ∷ VarId → Form → Form
    deriving (Eq, Ord, Generic)

future ∷ Form → Form
future ϕ = Truth `Until` ϕ

globally ∷ Form → Form
globally ϕ = Not (future (Not ϕ))

instance Show Form where
  show f = let str = case f of
                 (Truth `Until` ϕ)             → "F" ++ show ϕ
                 (Not (Truth `Until` (Not ϕ))) → "G" ++ show ϕ
                 Truth      → "⊤"
                 Not ϕ      → "¬" ++ show ϕ
                 And ϕ ψ    → show ϕ ++ " ∧ " ++ show ψ
                 Next ϕ     → "X" ++ show ϕ
                 Until ϕ ψ  → show ϕ ++ " U " ++ show ψ
                 Nom n      → show n
                 Var x      → [x]
                 At x ϕ     → "@" ++ (showAtId x) ++ "." ++ show ϕ
                 Bind x ϕ   → "↓" ++ [x] ++ "." ++ show ϕ
      in if isDeep f then enclose str else str
    where isDeep f = case f of
            Truth  → False
            Not ϕ  → isDeep ϕ
            Next ϕ → isDeep ϕ
            Nom _  → False
            Var _  → False
            _      → True
          enclose = (++")") ∘ ("("++)
          showAtId = either show (pure ∷ VarId → String)

instance Hashable Form
instance Binary Form
instance Serializable Form


type StateId = Int
type Prop = String
type Value = String

-- A state in a run.
data State = State {
    stateId   ∷ StateId     -- ^ A unique identifier.
  , stateVal  ∷ Value       -- ^ Data value associated with this state.
  , stateProp ∷ [Prop]      -- ^ List of propositions satisfied.
  , stateNext ∷ Maybe State -- ^ Successor.
  , statePrev ∷ Maybe State -- ^ Predecessor.
  }

instance Show State where
  show State{..} = show stateId ⧺ ":\t" ⧺ stateVal ⧺ "\t" ⧺ show stateProp

-- Eh.
getStateId = stateId

instance Eq State where
  (==) a b = (stateId a) == (stateId b)

instance Ord State where
  (<=) a b = (stateId a) <= (stateId b)

instance Hashable State where
  hashWithSalt s State{..} = hashWithSalt s stateId


-- A run, then, is a sequence of such states.
newtype Run = Run [State]

unRun ∷ Run → [State]
unRun (Run m) = m

getStateById ∷ Run → StateId → State
getStateById r sid = fromJust (lookupStateById r sid)

lookupStateById ∷ Run → StateId → Maybe State
lookupStateById (Run rs) sid = find ((== sid) ∘ stateId) rs

getPointedRun ∷ Run → State → [State]
getPointedRun (Run r) State{..} = drop stateId r

instance Show Run where
  show (Run xs) = unlines $ map show xs


-- We also have structure which is parsed from file and later converted to the "real" thing.
data ParsedState = ParsedState {
    parsedVal  ∷ Value
  , parsedProp ∷ [Prop]
  } deriving (Show, Generic)

instance Binary ParsedState
instance Serializable ParsedState

type ParsedRun = [ParsedState]
