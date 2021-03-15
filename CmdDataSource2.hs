{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module CmdDataSource2
  ( cmd
  , CommandFailed(..)
  , initDataSource
  ) where

import Prelude ()
import Haxl.Prelude

import Control.Exception hiding (throw)
import System.Exit
import Haxl.Core
import Data.Typeable
import Control.Concurrent
import Control.Monad (zipWithM, forever)
import Data.Time
import Text.Printf
import Data.Hashable
import Control.Concurrent.Async
import System.Process

-- -----------------------------------------------------------------------------
-- External API

cmd :: FilePath -> [String] -> GenHaxl u w ()
cmd prog args = do
  e <- dataFetch (RunCmdReq prog args)
  case e of
    ExitSuccess -> return ()
    ExitFailure _ -> throw (CommandFailed prog args e)


-- -----------------------------------------------------------------------------
-- Requests

data RunCmdReq a where
  RunCmdReq :: String -> [String] -> RunCmdReq ExitCode
  deriving Typeable

deriving instance Eq (RunCmdReq a)
deriving instance Show (RunCmdReq a)

instance ShowP RunCmdReq where showp = show

instance Hashable (RunCmdReq a) where
   hashWithSalt s (RunCmdReq prog args) = hashWithSalt s (0::Int,prog,args)

-- -----------------------------------------------------------------------------
-- Data source implementation

instance StateKey RunCmdReq where
  data State RunCmdReq = RunCmdState { logChan :: Chan String }

instance DataSourceName RunCmdReq where
  dataSourceName _ = "RunCmd"

instance DataSource u RunCmdReq where
  fetch = buildCmdFetch

initDataSource :: IO (State RunCmdReq)
initDataSource = do
  chan <- newChan
  forkIO (forever $ readChan chan >>= putStr)
  return RunCmdState { logChan = chan }

buildCmdFetch :: State RunCmdReq             -- current state
             -> Flags                        -- tracing verbosity, etc.
             -> u                            -- user environment
             -> PerformFetch RunCmdReq       --  response requests to fetch

buildCmdFetch RunCmdState { logChan = chan } _flags _user =
  SyncFetch $ batchFetch chan

batchFetch :: Chan String -> [BlockedFetch RunCmdReq] -> IO ()
batchFetch chan bfs = do
    t <- getCurrentTime
    mapM_ wait =<< zipWithM (\ n b -> async (fetch1 t chan n b)) [1..] bfs


fetch1 :: UTCTime -> Chan String -> Int -> BlockedFetch RunCmdReq -> IO ()
fetch1 t0 chan n (BlockedFetch (RunCmdReq prog args) rvar) = do
  writeChan chan $ printf "[%d] %s\n" n (unwords (prog:args))
  r <- Control.Exception.try $
         withCreateProcess (proc prog args) { delegate_ctlc = True } $
            \_ _ _ p -> waitForProcess p
  t1 <- getCurrentTime
  let t = realToFrac (diffUTCTime t1 t0) :: Double
  let status = case r of
                 Right ExitSuccess -> "OK"
                 Right (ExitFailure n) -> "exit(" ++ show n ++ ")"
                 Left e -> show e
  writeChan chan $ printf "[%d] %s %.2fs\n" n status t
  putResult rvar r


-- withCreateProcess :: CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
-- cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()

-- -----------------------------------------------------------------------------
-- Exceptions

data CommandFailed = CommandFailed String [String] ExitCode
  deriving (Typeable, Show)

instance Exception CommandFailed where
  toException = logicErrorToException
  fromException = logicErrorFromException