{-# LANGUAGE UnicodeSyntax, GADTs #-}
module Types where


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


-- The data type of hybrid Kripke structures.
data Model = Model {
    modelStates ∷ [StateId]
  , modelTrans  ∷ (StateId → [StateId]) }
