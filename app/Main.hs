{-# LANGUAGE RecordWildCards #-}
module Main where

import           Config
import qualified ModelCheck.Simple
import           Options (parser)
import           Options.Applicative (execParser)


main :: IO ()
main = execParser parser >>= loadConfig >>= run

run ∷ Config → IO ()
run (Left c) = runMaster c
run (Right c) = runSlave c

runMaster ∷ MasterConfig → IO ()
runMaster MasterConfig{..} = undefined

runSlave ∷ SlaveConfig → IO ()
runSlave SlaveConfig{..} = undefined
