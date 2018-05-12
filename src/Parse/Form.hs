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

constForm = choice [truth, var, nom, data']
unaryForm = choice [not, next, Parse.Form.future, Parse.Form.globally]
binaryForm = choice $ map try [and, or', impl, until]
binderForm = choice [at, bind]

truth = truthLex >> (return Truth)

not = Not <$> (notLex *> form)

and = liftA2 And form (andLex *> form)

or' = liftA2 boolOr form (orLex *> form)

impl = liftA2 boolImplies form (implLex *> form)

next = Next <$> (nextLex *> form)

future = Types.future <$> (futureLex *> form)

globally = Types.globally <$> (globallyLex *> form)

until = liftA2 Until form (untilLex *> form)

nom = Nom <$> stateIdLex

data' = Data <$> (dataLex *> varIdLex)

var = Var <$> varIdLex

at = liftA2 At (atLex *> varIdLex <* (char '.')) form

bind = liftA2 Bind (bindLex *> varIdLex <* (char '.')) form

stateIdLex  = many1 alphaNum
varIdLex    = lower
dataLex     = char '~'
truthLex    = char '⊤' <|> char 'T'
notLex      = char '¬' <|> char '-'
andLex      = char '∧' <|> char '&'
orLex       = char '∨' <|> char '|'
implLex     = char '→' <|> char '>'
nextLex     = char 'X'
futureLex   = char 'F'
globallyLex = char 'G'
untilLex    = char 'U'
atLex       = char '@'
bindLex     = char '↓' <|> char 'B'
