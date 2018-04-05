{-# LANGUAGE RecordWildCards #-}
module Master (spawnMaster) where

import           Config
import           Control.Distributed.Process
import           Control.Monad               (forM)
import           Data.Function.Unicode
import           Data.List                   (intercalate)
import           List
import           Monad.ModelCheck
import           Parse.Model
import           Slave
import           Types


spawnMaster ∷ MasterConfig → [NodeId] → Process ()
spawnMaster _ [] = error "No slave nodes appear to be running"
spawnMaster MasterConfig{model = m@ValidatedModel{..}, form = ϕ} slaves = do
  say $ "Discovered slave nodes (" ++ show (length slaves) ++ "): " ++ show slaves
  pids ← distribute (spawnSlave m ϕ)
  self ← getSelfPid
  mapM_ (`send` (self:pids)) pids
  res ← getResult (build m) ϕ
  liftIO $ putStrLn res
  return ()
    where distribute = let ss        = map parsedId validatedStates
                           workloads = partitionN (length slaves) ss
                       in forM (zip workloads slaves) ∘ uncurry

getResult ∷ Model → Form → Process String
getResult m ϕ = awaitResult m ϕ >>= return ∘ (renderResult ϕ)

awaitResult ∷ Model → Form → Process [(State, Bool)]
awaitResult m ϕ = newModelCheckState [] >>= stateServer ϕ >>= evalIxStateT (lookupAll m ϕ)

renderResult ∷ Form → [(State, Bool)] → String
renderResult ϕ res = intercalate "\n" ["Formula: " ++ show ϕ, header, body]
  where header = "All states satisfied: " ++ show (and (map snd res))
        body   = unlines $ map (\(s, r) → (stateId s) ++ ": " ++ (show r)) res
