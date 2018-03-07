module Parse.Form (Parse.Form.parse) where

import           Control.Applicative      hiding ((<|>))
import           Data.Function.Unicode
import           Prelude                  hiding (and, not, until)
import           Text.Parsec
import           Types


parse ∷ String → Either ParseError Form
parse = Text.Parsec.parse (form <* eof) "formula" ∘ enclose "(" ")"
  where enclose l r x = l ++ x ++ r

form =
  spaces *>
  ((between (char '(') (char ')') (binderForm <|> binaryForm)) <|> unaryForm <|> constForm)
  <* spaces

constForm = someOf [truth, nom, var]
unaryForm = someOf [not, future, past]
binaryForm = someOf [and, until, since]
binderForm = someOf [at, bind, exists]
someOf = choice ∘ map try

truth = truthLex >> (return Truth)

not = Not <$> (notLex *> form)

and = liftA2 And form (andLex *> form)

future = Future <$> (futureLex *> form)

past = Past <$> (pastLex *> form)

until = liftA2 Until form (untilLex *> form)

since = liftA2 Since form (sinceLex *> form)

nom = Nom <$> stateIdLex

var = Var <$> varIdLex

at = liftA2 At (atLex *> (st <|> vr) <* (char '.')) form
  where st = stateIdLex >>= return ∘ Left
        vr = varIdLex >>= return ∘ Right

bind = liftA2 Bind (bindLex *> varIdLex <* (char '.')) form

exists = liftA2 Exists (existsLex *> varIdLex <* (char '.')) form

stateIdLex = many1 alphaNum
varIdLex   = letter
truthLex   = char '⊤'
notLex     = char '¬'
andLex     = char '∧'
futureLex  = char 'F'
pastLex    = char 'P'
untilLex   = char 'U'
sinceLex   = char 'S'
atLex      = char '@'
bindLex    = char '↓'
existsLex  = char '∃'
