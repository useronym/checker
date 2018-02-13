{-# LANGUAGE UnicodeSyntax #-}
module Parse.Model where

import           Control.Applicative      hiding ((<|>))
import           Control.Category.Unicode
import           Data.ByteString          (readFile)
import           Data.Yaml
import           Prelude                  hiding (readFile)
import           Types


loadModel ∷ FilePath → IO (Either ParseException Model)
loadModel path = readFile path >>= return ∘ (fmap $ build ∘ validate) ∘ decodeEither'

build ∷ ParsedModel → Model
build = undefined

validate ∷ ParsedModel → ParsedModel
validate = id
