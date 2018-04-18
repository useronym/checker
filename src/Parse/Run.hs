{-# LANGUAGE RecordWildCards #-}
module Parse.Run where

import           Data.Either           (fromRight)
import           Data.Function.Unicode
import           Data.Maybe            (fromMaybe)
import           Text.Parsec           hiding (State)
import           Types


load ∷ FilePath → IO [ParsedState]
load path = ((either (error ∘ show) id) ∘ (Parse.Run.parse path)) <$> readFile path

parse ∷ FilePath → String → Either ParseError [ParsedState]
parse = Text.Parsec.parse (run <* eof)

build ∷ [ParsedState] → Run
build ss = let run = Run $ map (buildState run) (zip [0..] ss)
             in run
  where
    buildState rs (i, ParsedState{..}) = State
      { stateId   = i
      , stateVal  = parsedVal
      , stateProp = parsedProp
      , stateNext = lookupStateById rs (i+1)
      , statePrev = lookupStateById rs (i-1)
      }

run = sepBy1 pos newline

pos = do
  val ← word
  ps ← optionMaybe (oneOf " \t" *> props)
  return $ ParsedState val (fromMaybe [] ps)

props = between (char '[') (char ']') (word `sepBy` (blanks *> char ',' <* blanks))

word = many1 (alphaNum <|> oneOf "-_?!")

blanks = many (char ' ')
