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

constForm = someOf [truth, var]
unaryForm = someOf [not, next, Parse.Form.future, Parse.Form.globally]
binaryForm = someOf [and, until]
binderForm = someOf [at, bind]
someOf = choice ∘ map try

truth = truthLex >> (return Truth)

not = Not <$> (notLex *> form)

and = liftA2 And form (andLex *> form)

next = Next <$> (nextLex *> form)

future = Types.future <$> (futureLex *> form)

globally = Types.globally <$> (globallyLex *> form)

until = liftA2 Until form (untilLex *> form)

var = Var <$> varIdLex

at = liftA2 At (atLex *> vr <* (char '.')) form
  where vr = varIdLex >>= return ∘ Right

bind = liftA2 Bind (bindLex *> varIdLex <* (char '.')) form

varIdLex    = lower
truthLex    = char '⊤' <|> char 'T'
notLex      = char '¬' <|> char '~'
andLex      = char '∧' <|> char '&'
nextLex     = char 'X'
futureLex   = char 'F'
globallyLex = char 'G'
untilLex    = char 'U'
atLex       = char '@'
bindLex     = char '↓' <|> char 'B'
