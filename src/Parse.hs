{-# LANGUAGE UnicodeSyntax #-}
module Parse where

import           Control.Applicative      hiding ((<|>))
import           Control.Category.Unicode
import           Prelude                  hiding (and, not, until)
import           Text.Parsec
import           Types


parseForm ∷ String → Either ParseError Form
parseForm = parse (form <* eof) "formula" ∘ enclose "(" ")"
  where enclose l r x = l ++ x ++ r

form = spaces *> ((between (char '(') (char ')') (binderForm <|> binaryForm)) <|> unaryForm <|> constForm) <* spaces

constForm = someOf [truth, nom, var]
unaryForm = someOf [not, future, past]
binaryForm = someOf [and, until, since]
binderForm = someOf [at, bind, exists]
someOf = choice ∘ map try

truth = char '⊤' >> (return Truth)

not = Not <$> (char '¬' *> form)

and = liftA2 And form (char '∧' *> form)

future = Future <$> (char 'F' *> form)

past = Past <$> (char 'P' *> form)

until = liftA2 Until form (char 'U' *> form)

since = liftA2 Since form (char 'S' *> form)

nom = Nom <$> stateId

var = Var <$> varId

at = liftA2 At (char '@' *> (st <|> vr) <* (char '.')) form
  where st = stateId >>= return ∘ Left
        vr = varId >>= return ∘ Right

bind = liftA2 Bind (char '↓' *> varId <* (char '.')) form

exists = liftA2 Exists (char '∃' *> varId <* (char '.')) form

stateId = (many1 alphaNum)
varId = letter
