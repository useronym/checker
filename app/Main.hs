module Main where

import           Config
import qualified ModelCheck.Simple
import           Options (parser)
import           Options.Applicative (execParser)


main :: IO ()
main = execParser parser >>= loadConfig >>= run

run ∷ Config → IO ()
run = undefined

