{-# LANGUAGE RecordWildCards #-}
module Master (spawnMaster) where

import           Config
import           Control.Distributed.Process
import           Control.Monad               (forM)
import           Data.Function.Unicode
import           Data.List                   (nub, intercalate)
import           List
import           Monad.ModelCheck
import           Parse.Model
import           Slave
import           Syntax
import           System.IO                   (hFlush, hPutStrLn)
import           Types


spawnMaster ∷ MasterConfig → [NodeId] → Process ()
spawnMaster _ [] = error "No slave nodes appear to be running"
spawnMaster c@MasterConfig{model = m@ValidatedModel{..}, form = ϕ} slaves = do
  say $ "Discovered slave nodes (" ++ show (length slaves) ++ "): " ++ show slaves
  pids ← distribute (spawnSlave m ϕ)
  self ← getSelfPid
  mapM_ (`send` (self:pids)) pids
  monitorResult c
    where distribute = let ss        = map parsedId validatedStates
                           workloads = partitionN (length slaves) ss
                       in forM (zip workloads slaves) ∘ uncurry


monitorResult ∷ MasterConfig → Process ()
monitorResult MasterConfig{model = m@ValidatedModel{..}, output = out, form = ϕ} = do
  let exp = (length validatedStates) * (length $ nub $ subformulae ϕ)
  state ← newModelCheckState []
  final ← execIxStateT (writeProgress exp) state
  res   ← evalIxStateT (lookupAll (build m) ϕ) final
  liftIO $ do
    hPutStrLn out (renderResult ϕ res)
    hFlush out

writeProgress ∷ Int → ModelCheck AWrite ()
writeProgress count = untilFinished count $ do
  processInput
  s ← size
  liftIO $ putStrLn $ show s ++ "/" ++ show count

renderResult ∷ Form → [(State, Bool)] → String
renderResult ϕ res = intercalate "\n" ["State count: " ++ show (length res), "Formula: " ++ show ϕ, header, body]
  where header = "All states satisfied: " ++ show (and (map snd res))
        body   = unlines $ map (\(s, r) → (stateId s) ++ ": " ++ (show r)) res
