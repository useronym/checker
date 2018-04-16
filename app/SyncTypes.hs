{-# LANGUAGE DeriveGeneric     #-}
module SyncTypes where

import           Control.Distributed.Process
import           GHC.Generics                             (Generic)
import           Data.Binary                              (Binary)


data SlaveFinished = SlaveFinished ProcessId
  deriving (Generic)

instance Binary SlaveFinished

data SlaveQuit = SlaveQuit
  deriving (Generic)

instance Binary SlaveQuit
