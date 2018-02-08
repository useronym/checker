{-# LANGUAGE UnicodeSyntax #-}
module Parse where

import Prelude hiding (not, and)
import Control.Category.Unicode
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Types


form' ∷ Parsec String () Form
form' = form >>= (\ ϕ → eof >> return ϕ)

form = do
  spaces *> ((between (char '(') (char ')') forms) <|> truth <|> prop <|> not <|> future) <* spaces
  where forms = choice $ map try [and]

truth = char '⊤' >> (return Truth)

prop = Prop <$> letter

not = Not <$> (char '¬' *> form)

and = liftA2 And form (char '∧' *> form)

future = Future <$> (char 'F' *> form)
