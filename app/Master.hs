{-# LANGUAGE RecordWildCards #-}
module Master (spawnMaster) where

import           Config
import           Control.Distributed.Process
import           Control.Monad               (forM)
import           Data.Function.Unicode
import           Data.List                   (delete, intercalate, nub)
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
  mapM_ (`send` (self:pids)) pids
  monitorResult c pids
    where distribute = let ss        = map parsedId validatedStates
                           workloads = partitionN (length slaves) ss
                       in forM (zip workloads slaves) ∘ uncurry


monitorResult ∷ MasterConfig → [ProcessId] → Process ()
monitorResult MasterConfig{model = m@ValidatedModel{..}, output = out, form = ϕ} pids = do
  let exp = numEntries (length validatedStates) ϕ
  state ← newModelCheckState []
  final ← writeProgress exp state pids
  res   ← evalIxStateT (lookupAll (build m) ϕ) final
  liftIO $ do
    hPutStrLn out (renderResult ϕ res)
    hFlush out
  mapM_ (`send` SlaveQuit) pids

writeProgress ∷ Int → ModelCheckState AWrite → [ProcessId] → Process (ModelCheckState AWrite)
writeProgress count state pids = do
  mdone  ← expectTimeout 0 ∷ Process (Maybe SlaveFinished)
  state' ← execIxStateT (processInputTimeout 1000000) state
  s      ← evalIxStateT size state'
  liftIO $ putStrLn $ show s ++ "/" ++ show count
  let pids' = maybe pids handleDone mdone
  if (null pids')
    then return state'
    else writeProgress count state' pids'
  where handleDone (SlaveFinished p) = delete p pids

renderResult ∷ Form → [(State, Bool)] → String
renderResult ϕ res = intercalate "\n" ["State count: " ++ show (length res), "Formula: " ++ show ϕ, header, body]
  where header = "All states satisfied: " ++ show (and (map snd res))
        body   = unlines $ map (\(s, r) → (stateId s) ++ ": " ++ (show r)) res
