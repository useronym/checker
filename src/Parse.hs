{-# LANGUAGE UnicodeSyntax #-}
module Parse where

import           Control.Applicative      hiding ((<|>))
import           Control.Category.Unicode
import           Prelude                  hiding (and, not)
import           Text.Parsec
import           Types


parseForm ∷ String → Either ParseError Form
parseForm = parse (form <* eof) "formula" ∘ enclose "(" ")"
  where enclose l r x = l ++ x ++ r

form = spaces *> ((between (char '(') (char ')') binaryForm) <|> unaryForm <|> constForm) <* spaces

constForm = choice $ map try [truth, prop]
unaryForm = choice $ map try [not, future]
binaryForm = choice $ map try [and]

truth = char '⊤' >> (return Truth)

prop = Prop <$> letter

not = Not <$> (char '¬' *> form)

and = liftA2 And form (char '∧' *> form)

future = Future <$> (char 'F' *> form)
