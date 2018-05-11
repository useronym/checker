{-# LANGUAGE RecordWildCards #-}
module Master (spawnMaster) where

import           Config
import           Control.Distributed.Process
import           Control.Monad               (forM)
import           Data.Function.Unicode
import qualified Data.HashMap                as H
import           Data.List                   (delete, intercalate)
import           List
import           Monad.ModelCheck
import           Parse.Model
import           Slave
import           SyncTypes
import           Syntax
import           System.IO                   (hFlush, hPutStrLn)
import           Types


spawnMaster ∷ MasterConfig → [NodeId] → Process ()
spawnMaster _ [] = error "No slave nodes appear to be running"
spawnMaster c@MasterConfig{model = m@ValidatedModel{..}, form = ϕ} slaves = do
  say $ "Discovered slave nodes (" ++ show (length slaves) ++ "): " ++ show slaves
  self ← getSelfPid
  pids ← distribute (spawnSlave self m ϕ)
  mapM_ (`send` [self]) pids
  monitorResult c pids
    where distribute = let bm        = build m
                           runs      = makeAllRuns bm
                           workloads = partitionI (length slaves) (length runs)
                       in forM (zip workloads slaves) ∘ uncurry


monitorResult ∷ MasterConfig → [ProcessId] → Process ()
monitorResult MasterConfig{model = ValidatedModel{..}, output = out, form = ϕ} pids = do
  let maxRuns = tetrate (length validatedStates) (size ϕ)
  state ← newModelCheckState []
  final ← writeProgress maxRuns state pids
  res   ← evalIxStateT lookupFailed final
  liftIO $ do
    hPutStrLn out (renderResult ϕ res)
    hFlush out
  mapM_ (`send` SlaveQuit) pids

writeProgress ∷ Int → ModelCheckState AWrite → [ProcessId] → Process (ModelCheckState AWrite)
writeProgress count state pids = do
  mdone  ← expectTimeout 0 ∷ Process (Maybe SlaveFinished)
  state' ← execIxStateT (processInputTimeout 1000000) state
  s      ← evalIxStateT (withMap H.size) state'
  liftIO $ putStrLn $ show s ++ "/" ++ show count
  let pids' = maybe pids handleDone mdone
  if (null pids')
    then return state'
    else writeProgress count state' pids'
  where handleDone (SlaveFinished p) = delete p pids

renderResult ∷ Form → [SerRun] → String
renderResult ϕ fails = intercalate "\n" ["Formula: " ++ show ϕ, header, "Runs which violate property:", body]
  where header = "All checked runs satisfied: " ++ show (null fails)
        body   = unlines $ map show fails
